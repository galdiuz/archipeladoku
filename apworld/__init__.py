from typing import Any, Dict

from . import options, utils
from BaseClasses import Region, Location, Item, Tutorial, ItemClassification
from Options import OptionError
from collections import defaultdict
from worlds.AutoWorld import World


class ArchipeladokuWorld(World):
    game = "Archipeladoku"

    options_dataclass = options.ArchipeladokuOptions
    options: options.ArchipeladokuOptions

    item_name_to_id = utils.item_name_to_id
    location_name_to_id = utils.location_name_to_id

    block_unlock_order = defaultdict(list)
    clusters = defaultdict(dict)


    def generate_early(self):

        board_positions = utils.position_boards(
            self.options.block_size.value,
            self.options.number_of_boards.value,
        )

        grouped_positions = utils.group_positions(
            self.options.block_size.value,
            board_positions,
        )

        for idx, positions in grouped_positions.items():
            group_blocks = set([block for pos in positions for block in utils.build_blocks(self.options.block_size.value, pos)])
            cluster = utils.Cluster(
                id=idx,
                blocks=group_blocks,
                positions=set(positions)
            )

            self.clusters[self.player][idx] = cluster

        initial_unlock_count = utils.get_initial_unlock_count(self.options.block_size.value)

        self.block_unlock_order[self.player] = utils.build_block_unlock_order(
            initial_unlock_count,
            self.clusters[self.player],
            self.random,
        )


    def create_regions(self) -> None:

        menu = Region("Menu", self.player, self.multiworld)
        self.multiworld.regions.append(menu)

        initial_unlock_count = utils.get_initial_unlock_count(self.options.block_size.value)

        initial_blocks = set(self.block_unlock_order[self.player][:initial_unlock_count])
        block_cluster_map = defaultdict(list)

        for cluster in self.clusters[self.player].values():
            for block in cluster.blocks:
                block_cluster_map[block].append(cluster.blocks.difference(initial_blocks))

        cluster_unlock_requirements = utils.calculate_cluster_unlock_requirements(
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
                        state.has("Progressive Block", self.player, unlock_req) if unlock_req > 0 else True

                case options.BlockUnlocks.option_shuffled:
                    connection.access_rule = lambda state: True

                case _:
                    raise ValueError("Invalid block unlock option")

            for (row, col) in cluster.positions:
                loc = ArchipeladokuLocation(
                    self.player,
                    "Solve " + utils.board_name(row, col),
                    utils.board_id(row, col),
                    region,
                )
                region.locations.append(loc)

                match self.options.block_unlocks:
                    case options.BlockUnlocks.option_fixed:
                        loc.access_rule = lambda state: True

                    case options.BlockUnlocks.option_shuffled:
                        cluster_blocks = cluster.blocks.difference(initial_blocks)
                        def board_is_unlocked(state, cluster_blocks=cluster_blocks):
                            block_names = [utils.block_name(row, col) for (row, col) in cluster_blocks]
                            return state.has_all(block_names, self.player)

                        loc.access_rule = board_is_unlocked

                    case _:
                        raise ValueError("Invalid block unlock option")

            for (row, col) in cluster.blocks:
                if (row, col) in blocks_added:
                    continue

                blocks_added.add((row, col))

                loc = ArchipeladokuLocation(
                    self.player,
                    "Solve " + utils.block_name(row, col),
                    utils.block_id(row, col),
                    region,
                )
                region.locations.append(loc)

                match self.options.block_unlocks:
                    case options.BlockUnlocks.option_fixed:
                        loc.access_rule = lambda state: True

                    case options.BlockUnlocks.option_shuffled:
                        def block_is_unlocked(state, cluster_blocks=block_cluster_map[(row, col)]):
                            for blocks in cluster_blocks:
                                block_names = [utils.block_name(row, col) for (row, col) in blocks]
                                if state.has_all(block_names, self.player):
                                    return True

                            return False

                        loc.access_rule = block_is_unlocked

                    case _:
                        raise ValueError("Invalid block unlock option")

        victory_location = ArchipeladokuLocation(
            self.player,
            "Solve Everything",
            None,
            menu,
        )
        victory_item = ArchipeladokuItem(
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
                    state.has("Progressive Block", self.player, last_cluster_requirement)

            case options.BlockUnlocks.option_shuffled:
                victory_location.access_rule = lambda state: \
                    state.has_all(
                        [utils.block_name(row, col) for (row, col) in self.block_unlock_order[self.player][initial_unlock_count:] if row > 0],
                        self.player,
                    )

            case _:
                raise ValueError("Invalid block unlock option")

        self.multiworld.completion_condition[self.player] = lambda state: \
            state.has(victory_item.name, self.player)


    def create_items(self) -> None:

        initial_unlock_count = utils.get_initial_unlock_count(self.options.block_size.value)
        board_count = self.options.number_of_boards.value
        fillers = initial_unlock_count + board_count

        for ( row, col ) in self.block_unlock_order[self.player][initial_unlock_count:]:
            match self.options.block_unlocks:
                case options.BlockUnlocks.option_fixed:
                    item = self.create_item("Progressive Block")
                    self.multiworld.itempool.append(item)

                case options.BlockUnlocks.option_shuffled:
                    item = ArchipeladokuItem(
                        utils.block_name(row, col),
                        ItemClassification.progression,
                        utils.block_id(row, col),
                        self.player,
                    )
                    self.multiworld.itempool.append(item)

                case _:
                    raise ValueError("Invalid block unlock option")

        for _ in range(fillers):
            item = self.create_filler()
            self.multiworld.itempool.append(item)


    def fill_slot_data(self) -> Dict[str, Any]:

        initial_unlock_count = utils.get_initial_unlock_count(self.options.block_size.value)

        return {
            "blockSize": self.options.block_size.value,
            "blockUnlockOrder": self.block_unlock_order[self.player],
            "blockUnlocks": self.options.block_unlocks.value,
            "clusters": [cluster.positions for cluster in self.clusters[self.player].values()],
            "seed": self.random.getrandbits(32),
            "unlockedBlocks": initial_unlock_count,
        }


    def create_item(self, name: str) -> "ArchipeladokuItem":

        id = self.item_name_to_id.get(name)

        if id is None:
            raise ValueError(f"Invalid item name: {name}")

        if id < 100:
            classification = ItemClassification.filler
        elif id >= 100 and id < 200:
            classification = ItemClassification.progression
        elif id >= 200 and id < 300:
            classification = ItemClassification.useful
        elif id >= 400 and id < 500:
            classification = ItemClassification.trap
        elif id >= 1000000:
            classification = ItemClassification.progression
        else:
            raise ValueError(f"Invalid item id: {id}")

        return ArchipeladokuItem(
            name,
            classification,
            id,
            self.player,
        )


    def get_filler_item_name(self) -> str:

        weights = self.get_filler_weights()
        filler = self.random.choices(list(weights.keys()), weights=list(weights.values()))[0]

        return filler


    def get_filler_weights(self) -> Dict[str, int]:

        weights = {
            "Solve Selected Cell": self.options.solve_selected_cell_weight.value,
            "Solve Random Cell": self.options.solve_random_cell_weight.value,
        }

        if all(weight == 0 for weight in weights.values()):
            weights["Nothing"] = 1

        return weights


class ArchipeladokuLocation(Location):
    game = "Archipeladoku"


class ArchipeladokuItem(Item):
    game = "Archipeladoku"
