import math
import random
from dataclasses import dataclass
from collections import defaultdict


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


def get_initial_unlock_count(block_size: int, overlap_rows: int, overlap_cols: int) -> int:
    """Get the initial number of unlocked blocks."""

    [ block_rows, block_cols ] = block_size_to_dimensions(block_size)

    if overlap_rows % block_rows == 0 and overlap_cols % block_cols == 0:
        return block_size * 2 - 1

    else:
        return block_size * 2


def position_boards(block_size: int, overlap_rows: int, overlap_cols: int, number_of_boards: int) -> list[tuple[int, int]]:
    """Calculate positions for each board in the puzzle."""

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


@dataclass
class Cluster:
    id: int
    blocks: set[tuple[int, int]]
    positions: set[tuple[int, int]]


def build_block_unlock_order(
    unlocked: int,
    clusters: dict[int, Cluster],
    rng: random.Random,
) -> list[tuple[int, int]]:
    """Determine the order in which blocks are unlocked."""

    fillers = set([(-i, -i) for i in range(1, unlocked + 1)])
    remaining_blocks = set([block for cluster in clusters.values() for block in cluster.blocks])
    block_order_clusters = build_block_order_clusters(clusters)
    credits = unlocked
    order = []

    while len(block_order_clusters) > 0:
        weights = []
        for cluster in block_order_clusters.values():
            if cluster.remaining > credits:
                weights.append(0)

            elif (1, 1) in cluster.blocks:
                weights.append(100000000)

            else:
                weights.append(credits - cluster.remaining + 1)

        target = rng.choices(list(block_order_clusters.values()), weights=weights)[0]
        remaining_credits = credits - target.remaining
        remaining_blocks_without_target = remaining_blocks.difference(target.blocks)
        target_blocks_to_add = target.blocks.intersection(remaining_blocks)
        random_budget = rng.randint(0, min(remaining_credits, len(remaining_blocks_without_target)))
        random_blocks = rng.sample(list(remaining_blocks_without_target), k=random_budget)
        remaining_blocks_without_random = remaining_blocks_without_target.difference(set(random_blocks))
        shuffled_blocks = list(target_blocks_to_add) + random_blocks
        rng.shuffle(shuffled_blocks)

        credits = remaining_credits + len(target.blocks) - len(random_blocks)
        order.extend(shuffled_blocks)
        remaining_blocks = remaining_blocks_without_random

        if len(order) >= unlocked and len(fillers) > 0:
            remaining_blocks.update(fillers)
            fillers = set()

        del block_order_clusters[target.id]
        for cluster in block_order_clusters.values():
            cluster.remaining = len(cluster.blocks.intersection(remaining_blocks))

    order.extend(list(remaining_blocks))

    return order


@dataclass
class BlockOrderCluster:
    id: int
    blocks: set[tuple[int, int]]
    remaining: int


def build_block_order_clusters(
    clusters: dict[int, Cluster],
) -> dict[int, BlockOrderCluster]:
    """Build a mapping of clusters for use in block ordering."""

    remaining_blocks = set([block for cluster in clusters.values() for block in cluster.blocks])
    block_order_clusters = {}

    for cluster in clusters.values():
        blocks = cluster.blocks.intersection(remaining_blocks)
        remaining_blocks.difference_update(blocks)

        block_order_clusters[cluster.id] = BlockOrderCluster(
            id = cluster.id,
            blocks = blocks,
            remaining = len(blocks),
        )

    return block_order_clusters
