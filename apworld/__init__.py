from typing import Any, Dict

from . import board, options
from BaseClasses import Region, Location, Item, Tutorial, ItemClassification
from Options import OptionError
from collections import defaultdict
from worlds.AutoWorld import World


class ArchipeladokuWorld(World):
    game = "Archipeladoku"

    options_dataclass = options.ArchipeladokuOptions
    options: options.ArchipeladokuOptions

    item_name_to_id = {}
    location_name_to_id = {}

    block_unlock_order = defaultdict(list)
    clusters = defaultdict(dict)


    def generate_early(self):
        board_positions = board.position_boards(
            self.options.block_size.value,
            self.options.number_of_boards.value,
        )

        grouped_positions = board.group_positions(
            self.options.block_size.value,
            board_positions,
        )

        for idx, positions in grouped_positions.items():
            group_blocks = set([block for pos in positions for block in board.build_blocks(self.options.block_size.value, pos)])
            cluster = board.Cluster(
                id=idx,
                blocks=group_blocks,
                positions=set(positions)
            )

            self.clusters[self.player][idx] = cluster

        initial_unlock_count = board.get_initial_unlock_count(self.options.block_size.value)

        self.block_unlock_order[self.player] = board.build_block_unlock_order(
            initial_unlock_count,
            self.clusters[self.player],
            self.random,
        )


    def create_regions(self):
        menu = Region("Menu", self.player, self.multiworld)
        self.multiworld.regions.append(menu)

        initial_unlock_count = board.get_initial_unlock_count(self.options.block_size.value)

        initial_blocks = set(self.block_unlock_order[self.player][:initial_unlock_count])
        block_cluster_map = defaultdict(list)

        for cluster in self.clusters[self.player].values():
            for block in cluster.blocks:
                block_cluster_map[block].append(cluster.blocks.difference(initial_blocks))

        cluster_unlock_requirements = board.calculate_cluster_unlock_requirements(
            self.clusters[self.player],
            self.block_unlock_order[self.player],
            initial_unlock_count,
        )

        blocks_added = set()

        for cluster in self.clusters[self.player].values():
            # TODO: Name as Board/Cluster based on number of positions
            region = Region(f"Board {cluster.id}", self.player, self.multiworld)
            self.multiworld.regions.append(region)

            connection = menu.connect(region)

            match self.options.block_unlocks:
                case options.BlockUnlocks.option_fixed:
                    connection.access_rule = lambda state, unlock_req=cluster_unlock_requirements[cluster.id]: \
                        state.has("Block", self.player, unlock_req) if unlock_req > 0 else True

                case options.BlockUnlocks.option_shuffled:
                    connection.access_rule = lambda state: True

                case _:
                    raise ValueError("Invalid block unlock option")

            for (row, col) in cluster.blocks:
                if (row, col) in blocks_added:
                    continue

                blocks_added.add((row, col))

                loc = Location(
                    self.player,
                    f"Solve {self.block_name(row, col)}",
                    ItemClassification.progression,
                    region,
                )
                loc.address = 1000000 + row * 1000 + col
                region.locations.append(loc)

                self.location_name_to_id[loc.name] = loc.address

                match self.options.block_unlocks:
                    case options.BlockUnlocks.option_fixed:
                        loc.access_rule = lambda state: True

                    case options.BlockUnlocks.option_shuffled:
                        def block_is_unlocked(state, cluster_blocks=block_cluster_map[(row, col)]):
                            for blocks in cluster_blocks:
                                block_names = [self.block_name(row, col) for (row, col) in blocks]
                                if state.has_all(block_names, self.player):
                                    return True

                            return False

                        loc.access_rule = block_is_unlocked

                    case _:
                        raise ValueError("Invalid block unlock option")

        victory_location = Location(
            self.player,
            "Solve Everything",
            None,
            menu,
        )
        victory_item = Item(
            "Victory",
            ItemClassification.progression,
            None,
            self.player,
        )
        victory_location.place_locked_item(victory_item)

        menu.locations.append(victory_location)

        match self.options.block_unlocks:
            case options.BlockUnlocks.option_fixed:
                last_cluster_requirement = max(cluster_unlock_requirements.values())
                victory_location.access_rule = lambda state: \
                    state.has("Block", self.player, last_cluster_requirement)

            case options.BlockUnlocks.option_shuffled:
                victory_location.access_rule = lambda state: \
                    state.has_all(
                        [self.block_name(row, col) for (row, col) in self.block_unlock_order[self.player][initial_unlock_count:] if row > 0],
                        self.player,
                    )

            case _:
                raise ValueError("Invalid block unlock option")

        self.multiworld.completion_condition[self.player] = lambda state: \
            state.has(victory_item.name, self.player)

        # print("Locations", self.multiworld.regions.location_cache)
        print("Locations", len(self.multiworld.regions.location_cache[self.player]))


    def create_items(self):
        initial_unlock_count = board.get_initial_unlock_count(self.options.block_size.value)

        for _ in range(initial_unlock_count):
            item = Item(
                "Filler",
                ItemClassification.filler,
                2,
                self.player,
            )
            self.multiworld.itempool.append(item)
            self.item_name_to_id[item.name] = item.code

        for ( row, col ) in self.block_unlock_order[self.player][initial_unlock_count:]:
            match self.options.block_unlocks:
                case options.BlockUnlocks.option_fixed:
                    item = Item(
                        "Block",
                        ItemClassification.progression,
                        1,
                        self.player,
                    )
                    self.multiworld.itempool.append(item)
                    self.item_name_to_id[item.name] = item.code

                case options.BlockUnlocks.option_shuffled:
                    item = Item(
                        self.block_name(row, col),
                        ItemClassification.progression,
                        2000000 + row * 1000 + col,
                        self.player,
                    )
                    self.multiworld.itempool.append(item)
                    self.item_name_to_id[item.name] = item.code

                case _:
                    raise ValueError("Invalid block unlock option")

        # print("Items", self.multiworld.itempool)
        print("Items", len(self.multiworld.itempool))


    def fill_slot_data(self) -> Dict[str, Any]:
        initial_unlock_count = board.get_initial_unlock_count(self.options.block_size.value)

        return {
            "blockSize": self.options.block_size.value,
            "blockUnlockOrder": self.block_unlock_order[self.player],
            "clusters": [cluster.positions for cluster in self.clusters[self.player].values()],
            "seed": self.random.getrandbits(32),
            "unlockedBlocks": initial_unlock_count,
        }


    def block_name(self, row: int, col: int) -> str:
        return f"Block {self.row_to_label(row)}{col}"


    def row_to_label(self, row: int) -> str:
        chars = [
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M',
            'N', 'P', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
        ]
        base = len(chars)
        label = ""

        while row > 0:
            rem = (row - 1) % base
            row = (row - 1) // base
            label = chars[rem] + label

        return label
