from dataclasses import dataclass
from Options import Range, PerGameCommonOptions, Choice


class BlockSize(Choice):
    """The size of a single block (and the width of a row/column)."""
    display_name = "Block Size"
    option_4 = 4
    option_6 = 6
    option_8 = 8
    option_9 = 9
    option_12 = 12
    option_16 = 16
    default = 9


class NumberOfBoards(Range):
    """How many boards to generate in the archipelago."""
    display_name = "Number of Boards"
    range_start = 3
    range_end = 98
    default = 5


class BlockUnlocks(Choice):
    """The type of progression for unlocking blocks.
    - Fixed: Each block is unlocked in a predetermined order. Smoother progression.
    - Shuffled: Each block is unlocked by a specific check. More chaotic progression.
    """
    display_name = "Block Unlocks"
    option_fixed = "fixed"
    option_shuffled = "shuffled"
    default = "shuffled"


class SolveSelectedCellWeight(Range):
    """Weight (chance) that Solve Selected Cell is selected for a filler item."""
    display_name = "Filler Weight: Solve Selected Cell"
    range_start = 0
    range_end = 100
    default = 30


class SolveRandomCellWeight(Range):
    """Weight (chance) that Solve Random Cell is selected for a filler item."""
    display_name = "Filler Weight: Solve Random Cell"
    range_start = 0
    range_end = 100
    default = 70


@dataclass
class ArchipeladokuOptions(PerGameCommonOptions):
    block_size: BlockSize
    number_of_boards: NumberOfBoards
    block_unlocks: BlockUnlocks
    solve_selected_cell_weight: SolveSelectedCellWeight
    solve_random_cell_weight: SolveRandomCellWeight
