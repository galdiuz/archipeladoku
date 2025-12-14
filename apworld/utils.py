import math
import random
from dataclasses import dataclass
from collections import defaultdict


@dataclass
class Cluster:
    id: int
    blocks: set[tuple[int, int]]
    positions: set[tuple[int, int]]


def block_size_to_dimensions(block_size: int) -> (int, int):
    """Convert block size to board dimensions (rows, columns)."""

    match block_size:
        case 4: return (2, 2)
        case 6: return (2, 3)
        case 8: return (2, 4)
        case 9: return (3, 3)
        case 12: return (3, 4)
        case 16: return (4, 4)
        case _: raise ValueError("Unsupported block size")


def block_size_to_overlap(block_size: int) -> (int, int):
    """Convert block size to default overlap (rows, columns)."""

    match block_size:
        case 4: return (1, 1)
        case 6: return (2, 2)
        case 8: return (2, 2)
        case 9: return (3, 3)
        case 12: return (3, 4)
        case 16: return (4, 4)
        case _: raise ValueError("Unsupported block size")


def get_filler_count(block_size: int, number_of_boards: int) -> int:
    """Calculate the number of filler items needed."""

    return sum([
        block_size, # Initial blocks
        number_of_boards, # One per board
        number_of_boards * block_size * 2, # One per row and column
    ])


def position_boards(block_size: int, number_of_boards: int) -> list[tuple[int, int]]:
    """Calculate positions for each board in the puzzle."""

    [ overlap_rows, overlap_cols ] = block_size_to_overlap(block_size)

    def spots_in_grid(side: int) -> int:
        return math.ceil((side * side) / 2.0)

    def find_side_length(side: int) -> int:
        if spots_in_grid(side) >= number_of_boards:
            return side
        else:
            return find_side_length(side + 1)

    def is_corner_overlap(row: int, col: int) -> bool:
        return (row + col) % 2 == 0

    def map_to_cell(row: int, col: int) -> tuple[int, int]:
        return (row * (block_size - overlap_rows) + 1, col * (block_size - overlap_cols) + 1)

    grid_side_length = find_side_length(1)

    all_grid_coords = []
    for row in range(grid_side_length):
        for col in range(grid_side_length):
            all_grid_coords.append((row, col))

    corner_overlap = [pos for pos in all_grid_coords if is_corner_overlap(*pos)]
    correct_amount = corner_overlap[:number_of_boards]
    mapped_to_cells = [map_to_cell(*pos) for pos in correct_amount]

    sorted(mapped_to_cells, key=lambda x: max(x[0], x[1]))
    sorted(mapped_to_cells, key=lambda x: x[0] + x[1])

    return mapped_to_cells


def build_blocks(block_size: int, board_position: tuple[int, int]) -> set[tuple[int, int]]:
    """Generate the set of blocks for a given board position."""

    [ block_rows, block_cols ] = block_size_to_dimensions(block_size)
    (board_row, board_col) = board_position

    blocks = set()

    for row in range(block_cols):
        for col in range(block_rows):
            block_row = board_row + row * block_rows
            block_col = board_col + col * block_cols
            blocks.add((block_row, block_col))

    return blocks


def group_positions(block_size: int, positions: list[tuple[int, int]]) -> dict[int, list[tuple[int, int]]]:
    """Group board positions into clusters based on block size."""

    return dict([(idx + 1, [pos]) for idx, pos in enumerate(positions)])  # Placeholder implementation


def build_block_unlock_order(
    block_size: int,
    number_of_boards: int,
    clusters: dict[int, Cluster],
    rng: random.Random,
) -> list[tuple[int, int]]:
    """Determine the order in which blocks are unlocked."""

    filler_count = get_filler_count(block_size, number_of_boards)
    fillers = set([(-i, -i) for i in range(1, filler_count + 1)])
    assigned_blocks = set()
    remaining_blocks = set([block for cluster in clusters.values() for block in cluster.blocks])
    block_order_clusters = build_block_order_clusters(block_size, clusters)
    credits = block_size
    order = []

    while len(block_order_clusters) > 0:
        weights = []
        for cluster in block_order_clusters.values():
            remaining = len(cluster.blocks.intersection(remaining_blocks))
            if remaining > credits:
                weights.append(0)

            elif (1, 1) in cluster.blocks:
                weights.append(100000000)

            else:
                weights.append(credits - remaining + 1)

        target = rng.choices(list(block_order_clusters.values()), weights=weights)[0]
        remaining_credits = credits - len(target.blocks)
        remaining_blocks_without_target = remaining_blocks.difference(target.blocks)
        target_blocks_to_add = target.blocks.intersection(remaining_blocks)
        random_budget = rng.randint(0, min(remaining_credits, len(remaining_blocks_without_target)))
        random_blocks = rng.sample(list(remaining_blocks_without_target), k=random_budget)
        remaining_blocks_without_random = remaining_blocks_without_target.difference(set(random_blocks))
        shuffled_blocks = list(target_blocks_to_add) + random_blocks
        rng.shuffle(shuffled_blocks)

        credits = remaining_credits + len(target.blocks) + target.reward - len(random_blocks)
        order.extend(shuffled_blocks)
        remaining_blocks = remaining_blocks_without_random

        if len(fillers) > 0:
            remaining_blocks.update(fillers)
            fillers = set()

        assigned_blocks.update(target_blocks_to_add)
        del block_order_clusters[target.id]
        for cluster in block_order_clusters.values():
            cluster.blocks.difference_update(assigned_blocks)

    order = [block for block in order if block[0] >= 0]

    return order


@dataclass
class BlockOrderCluster:
    id: int
    blocks: set[tuple[int, int]]
    reward: int


def build_block_order_clusters(
    block_size: int,
    clusters: dict[int, Cluster],
) -> dict[int, BlockOrderCluster]:
    """Build a mapping of clusters for use in block ordering."""

    block_order_clusters = {}

    for cluster in clusters.values():
        block_order_clusters[cluster.id] = BlockOrderCluster(
            id = cluster.id,
            blocks = cluster.blocks.copy(),
            reward = len(cluster.positions) + len(cluster.positions) * block_size * 2,
        )

    return block_order_clusters


def calculate_cluster_unlock_requirements(
    clusters: dict[int, Cluster],
    block_unlock_order: list[tuple[int, int]],
    initial_unlock_count: int,
) -> dict[int, int]:
    """Calculate the number of blocks required to unlock each cluster."""

    block_to_index = {block: idx for idx, block in enumerate(block_unlock_order)}
    cluster_requirements = {}

    for cluster in clusters.values():
        indices = [block_to_index[block] for block in cluster.blocks if block in block_to_index]
        if len(indices) == 0:
            cluster_requirements[cluster.id] = 0
        else:
            cluster_requirements[cluster.id] = max(0, max(indices) + 1 - initial_unlock_count)

    return cluster_requirements


def block_id(row: int, col: int) -> int:
    return 1000000 + row * 1000 + col


def block_name(row: int, col: int) -> str:
    return f"Solve Block {row_to_label(row)}{col}"


def block_item_name(row: int, col: int) -> str:
    return f"Block {row_to_label(row)}{col}"


def row_id(row: int, col: int) -> int:
    return 2000000 + row * 1000 + col


def row_name(row: int, col: int) -> str:
    return f"Solve Row {row_to_label(row)}{col}"


def col_id(row: int, col: int) -> int:
    return 3000000 + row * 1000 + col


def col_name(row: int, col: int) -> str:
    return f"Solve Column {row_to_label(row)}{col}"


def board_id(row: int, col: int) -> int:
    return 4000000 + row * 1000 + col


def board_name(row: int, col: int) -> str:
    return f"Solve Board {row_to_label(row)}{col}"


def row_to_label(row: int) -> str:
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


item_name_to_id = {
    # 0xx: Filler Items
    "Solve Random Cell": 1,
    "Nothing": 99,
    # 1xx: Progression Items
    "Progressive Block": 101,
    # 2xx: Useful Items
    "Solve Selected Cell": 201,
    # 4xx: Trap Items
    # 1xxxyyy: Block Items, row xxx, col yyy, added below
}
location_name_to_id = {
    # 1xxxyyy: Solve Block Locations, row xxx, col yyy, added below
    # 1xxxyyy: Solve Row Locations, row xxx, col yyy, added below
    # 1xxxyyy: Solve Column Locations, row xxx, col yyy, added below
    # 4xxxyyy: Solve Board Locations, row xxx, col yyy, added below
}
max_width = 180 # Supports up to 98 blocks of size 16x16 with overlaps
for row in range(1, max_width):
    for col in range(1, max_width):
        item_name_to_id[block_item_name(row, col)] = block_id(row, col)
        location_name_to_id[block_name(row, col)] = block_id(row, col)
        location_name_to_id[row_name(row, col)] = row_id(row, col)
        location_name_to_id[col_name(row, col)] = col_id(row, col)
        location_name_to_id[board_name(row, col)] = board_id(row, col)
