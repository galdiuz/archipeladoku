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

        self.block_unlock_order[self.player] = utils.build_block_unlock_order(
            self.options.block_size.value,
            self.options.number_of_boards.value,
            self.clusters[self.player],
            self.random,
        )


    def create_regions(self) -> None:

        menu = Region("Menu", self.player, self.multiworld)
        self.multiworld.regions.append(menu)

        initial_unlock_count = self.options.block_size.value
        initial_blocks = set(self.block_unlock_order[self.player][:initial_unlock_count])
        cluster_unlock_requirements = utils.calculate_cluster_unlock_requirements(
            self.clusters[self.player],
            self.block_unlock_order[self.player],
            initial_unlock_count,
        )

        block_region_map = {}
        block_cluster_map = defaultdict(list)
        for cluster in self.clusters[self.player].values():
            for block in cluster.blocks:
                for position in cluster.positions:
                    block_cluster_map[block].append(position)

        for cluster in self.clusters[self.player].values():
            region = Region(f"Board {cluster.id}", self.player, self.multiworld)
            self.multiworld.regions.append(region)
            connection = menu.connect(region)

            match self.options.progression:
                case options.Progression.option_fixed:
                    connection.access_rule = lambda state, unlock_req=cluster_unlock_requirements[cluster.id]: \
                        state.has("Progressive Block", self.player, unlock_req) if unlock_req > 0 else True

                case options.Progression.option_shuffled:
                    cluster_blocks = cluster.blocks.difference(initial_blocks)
                    block_names = [utils.block_item_name(row, col) for (row, col) in cluster_blocks]

                    connection.access_rule = lambda state, block_names=block_names: \
                        state.has_all(block_names, self.player)

                case _:
                    raise ValueError("Invalid block unlock option")

            # Add board locations
            for (row, col) in cluster.positions:
                loc = ArchipeladokuLocation(
                    self.player,
                    utils.board_name(row, col),
                    utils.board_id(row, col),
                    region,
                )
                region.locations.append(loc)

                # Add row and column locations
                for offset in range(self.options.block_size.value):
                    loc = ArchipeladokuLocation(
                        self.player,
                        utils.row_name(row + offset, col),
                        utils.row_id(row + offset, col),
                        region,
                    )
                    region.locations.append(loc)

                    loc = ArchipeladokuLocation(
                        self.player,
                        utils.col_name(row, col + offset),
                        utils.col_id(row, col + offset),
                        region,
                    )
                    region.locations.append(loc)

            # Add block locations
            for (row, col) in cluster.blocks:
                block_clusters = block_cluster_map[(row, col)]
                if block_region_map.get((row, col)) is None:
                    if len(block_clusters) > 1:
                        block_region = Region(
                            f"Block {row},{col} Overlap",
                            self.player,
                            self.multiworld,
                        )
                        self.multiworld.regions.append(block_region)
                        connection = region.connect(block_region)
                        block_region_map[(row, col)] = block_region

                    else:
                        block_region = region

                    loc = ArchipeladokuLocation(
                        self.player,
                        utils.block_name(row, col),
                        utils.block_id(row, col),
                        block_region,
                    )
                    block_region.locations.append(loc)

                elif len(block_clusters) > 1:
                    block_region = block_region_map[(row, col)]
                    connection = region.connect(block_region)

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

        match self.options.progression:
            case options.Progression.option_fixed:
                last_cluster_requirement = max(cluster_unlock_requirements.values())
                victory_location.access_rule = lambda state: \
                    state.has("Progressive Block", self.player, last_cluster_requirement)

            case options.Progression.option_shuffled:
                victory_location.access_rule = lambda state: \
                    state.has_all(
                        [utils.block_item_name(row, col) for (row, col) in self.block_unlock_order[self.player][initial_unlock_count:] if row > 0],
                        self.player,
                    )

            case _:
                raise ValueError("Invalid block unlock option")

        self.multiworld.completion_condition[self.player] = lambda state: \
            state.has(victory_item.name, self.player)


    def create_items(self) -> None:

        initial_unlock_count = self.options.block_size.value
        fillers = utils.get_filler_count(
            self.options.block_size.value,
            self.options.number_of_boards.value,
        )

        for ( row, col ) in self.block_unlock_order[self.player][initial_unlock_count:]:
            match self.options.progression:
                case options.Progression.option_fixed:
                    item = self.create_item("Progressive Block")
                    self.multiworld.itempool.append(item)

                case options.Progression.option_shuffled:
                    item = ArchipeladokuItem(
                        utils.block_item_name(row, col),
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

        return {
            "blockSize": self.options.block_size.value,
            "blockUnlockOrder": self.block_unlock_order[self.player],
            "clusters": [cluster.positions for cluster in self.clusters[self.player].values()],
            "difficulty": self.options.difficulty.value,
            "locationScouting": self.options.location_scouting.value,
            "progression": self.options.progression.value,
            "seed": self.random.getrandbits(32),
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
            "Remove Random Candidate": self.options.remove_random_candidate_weight.value,
            "Nothing": self.options.nothing_weight.value,
        }

        if all(weight == 0 for weight in weights.values()):
            weights["Nothing"] = 1

        return weights


class ArchipeladokuLocation(Location):
    game = "Archipeladoku"


class ArchipeladokuItem(Item):
    game = "Archipeladoku"
