from typing import Any

from . import options, utils
from BaseClasses import CollectionState, Item, ItemClassification, Location, Region
from Options import OptionError
from collections import defaultdict
from worlds.AutoWorld import World
import Fill


class ArchipeladokuWorld(World):
    game = "Archipeladoku"

    options_dataclass = options.ArchipeladokuOptions
    options: options.ArchipeladokuOptions

    item_name_to_id = utils.item_name_to_id
    location_name_to_id = utils.location_name_to_id

    block_unlock_order = defaultdict(list)
    clusters = defaultdict(dict)
    pre_fill_items = []
    filler_counts = defaultdict(dict)
    target_pre_fill_nothing_count = defaultdict(int)


    def generate_early(self):

        board_positions = utils.position_boards(
            self.options.block_size.value,
            self.options.boards_per_cluster.value,
            utils.get_number_of_boards(
                self.options.block_size.value,
                self.options.number_of_boards.value,
            ),
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
            utils.get_number_of_boards(
                self.options.block_size.value,
                self.options.number_of_boards.value,
            ),
            self.clusters[self.player],
            self.random,
        )

        filler_counts = utils.get_filler_counts(self.options)
        self.filler_counts[self.player] = filler_counts
        pre_fill_nothings = filler_counts.get("Nothing", 0) * self.options.pre_fill_nothings_percent // 100

        if pre_fill_nothings > 0 and self.multiworld.players > 1:
            self.target_pre_fill_nothing_count[self.player] = pre_fill_nothings


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
                    raise ValueError("Invalid progression option")

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
                raise ValueError("Invalid progression option")

        self.multiworld.completion_condition[self.player] = lambda state: \
            state.has(victory_item.name, self.player)


    def create_items(self) -> None:

        initial_unlock_count = self.options.block_size.value
        items = []

        for ( row, col ) in self.block_unlock_order[self.player][initial_unlock_count:]:
            match self.options.progression:
                case options.Progression.option_fixed:
                    item = self.create_item("Progressive Block")
                    items.append(item)

                case options.Progression.option_shuffled:
                    item = self.create_item(utils.block_item_name(row, col))
                    items.append(item)

                case _:
                    raise ValueError("Invalid progression option")

        fillers = self.filler_counts[self.player]
        target_pre_fill_nothings = self.target_pre_fill_nothing_count[self.player]
        added_pre_fill_nothings = 0

        for item_name, count in fillers.items():
            for _ in range(count):
                item = self.create_item(item_name)

                if item_name == "Nothing" and added_pre_fill_nothings < target_pre_fill_nothings:
                    self.pre_fill_items.append(item)
                    added_pre_fill_nothings += 1

                else:
                    items.append(item)

        self.multiworld.itempool += items


    def get_pre_fill_items(self) -> list["Item"]:

        return self.pre_fill_items


    def pre_fill(self) -> None:

        if not self.pre_fill_items:
            return

        # Set up state, copied from ladx
        partial_all_state = CollectionState(self.multiworld)
        # Collect every item from the item pool and every pre-fill item like MultiWorld.get_all_state, except our own pre-fill items.
        for item in self.multiworld.itempool:
            partial_all_state.collect(item, prevent_sweep=True)
        for player in self.multiworld.player_ids:
            if player == self.player:
                # Don't collect the items we're about to place.
                continue
            world = self.multiworld.worlds[player]
            for item in world.get_pre_fill_items():
                partial_all_state.collect(item, prevent_sweep=True)
        partial_all_state.sweep_for_advancements()

        sphere_zero_locs = self.multiworld.get_reachable_locations(
            CollectionState(self.multiworld),
            self.player
        )
        locations_to_fill = [
            loc for loc in self.multiworld.get_unfilled_locations(self.player)
            if loc not in sphere_zero_locs
            and loc.name not in self.options.priority_locations.value
        ]
        self.random.shuffle(locations_to_fill)

        items_to_fill = self.pre_fill_items
        self.random.shuffle(items_to_fill)

        Fill.fill_restrictive(
            self.multiworld,
            partial_all_state,
            locations_to_fill,
            items_to_fill,
            lock=True,
            single_player_placement=True,
            allow_partial=True,
            name=f"Archipeladoku Pre-Fill for Player {self.player}"
        )


    def fill_slot_data(self) -> dict[str, Any]:

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


    def get_filler_weights(self) -> dict[str, int]:

        weights = {
            "Solve Selected Cell": self.options.solve_selected_cell_ratio.value,
            "Solve Random Cell": self.options.solve_random_cell_ratio.value,
            "Remove Random Candidate": self.options.remove_random_candidate_ratio.value,
            "Emoji Trap": self.options.emoji_trap_ratio.value,
            "Nothing": self.get_nothing_weight(),
        }

        if all(weight == 0 for weight in weights.values()):
            weights["Nothing"] = 1

        return weights


    def get_nothing_weight(self) -> int:

        weight = self.options.block_size.value * 200 + 100 \
            - self.options.solve_selected_cell_ratio.value \
            - self.options.solve_random_cell_ratio.value \
            - self.options.remove_random_candidate_ratio.value \

        return max(0, weight)


class ArchipeladokuLocation(Location):
    game = "Archipeladoku"


class ArchipeladokuItem(Item):
    game = "Archipeladoku"
