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

class OverlapRows(Range):
    """How many cells overlap between adjacent boards."""
    display_name = "Overlap"
    range_start = 0
    range_end = 4
    default = 3

class OverlapCols(Range):
    """How many cells overlap between adjacent boards."""
    display_name = "Overlap"
    range_start = 0
    range_end = 4
    default = 3

class NumberOfBoards(Range):
    """How many boards to generate in the archipelago."""
    display_name = "Number of Boards"
    range_start = 1
    range_end = 50
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

@dataclass
class ArchipeladokuOptions(PerGameCommonOptions):
    block_size: BlockSize
    overlap_rows: OverlapRows
    overlap_cols: OverlapCols
    number_of_boards: NumberOfBoards
    block_unlocks: BlockUnlocks
