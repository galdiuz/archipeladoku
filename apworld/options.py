from dataclasses import dataclass
from Options import Choice, NamedRange, PerGameCommonOptions, Range


class BlockSize(Choice):
    """The size of a single block (and the width/height of each board)."""
    display_name = "Block Size"
    option_4 = 4
    option_6 = 6
    option_8 = 8
    option_9 = 9
    option_12 = 12
    option_16 = 16
    default = 9


class BoardsPerCluster(NamedRange):
    """How many boards to put in each cluster of overlapping boards.
    Recommended values are 1 (no grouping), 5, 8, 13, or 100 (group all).
    """
    display_name = "Boards per Cluster"
    range_start = 1
    range_end = 100
    default = 5
    special_range_names = {
        "no_grouping": 1,
        "square_of_5": 5,
        "square_of_8": 8,
        "square_of_13": 13,
        "group_all": 100,
    }


class NumberOfBoards(Range):
    """How many boards to generate. Maximum depend on block size:
    - 4-9: 100 boards
    - 12: 64 boards
    - 16: 36 boards
    """
    display_name = "Number of Boards"
    range_start = 3
    range_end = 100
    default = 5


class Difficulty(Choice):
    """The overall difficulty level. Solving techniques required:
    - Beginner: Naked/hidden singles.
    - Easy: Pointing pairs, box line reduction.
    - Medium: Naked pairs/triples.
    - Hard: Hidden pairs/triples.
    """
    display_name = "Difficulty"
    option_beginner = 1
    option_easy = 2
    option_medium = 3
    option_hard = 4
    default = 2


class Progression(Choice):
    """The type of progression for unlocking blocks.
    - Fixed: Each block is unlocked in a predetermined order. Smoother progression.
    - Shuffled: Each block is unlocked by a specific check. More chaotic progression.
    """
    display_name = "Block Unlocks"
    option_fixed = "fixed"
    option_shuffled = "shuffled"
    default = "shuffled"


class LocationScouting(Choice):
    """How scouting of locations is handled.
    - Auto: Locations are scouted automatically when fully revealed.
    - Manual: Locations can be scouted manually.
    - Disabled: Locations cannot be scouted.
    """
    display_name = "Location Scouting"
    option_auto = "auto"
    option_manual = "manual"
    option_disabled = "disabled"
    default = "manual"


class SolveSelectedCellWeight(Range):
    """Weight (chance) that Solve Selected Cell is selected for a filler item."""
    display_name = "Filler Weight: Solve Selected Cell"
    range_start = 0
    range_end = 100
    default = 5


class SolveRandomCellWeight(Range):
    """Weight (chance) that Solve Random Cell is selected for a filler item."""
    display_name = "Filler Weight: Solve Random Cell"
    range_start = 0
    range_end = 100
    default = 10


class RemoveRandomCandidateWeight(Range):
    """Weight (chance) that Remove Random Candidate is selected for a filler item."""
    display_name = "Filler Weight: Remove Random Candidate"
    range_start = 0
    range_end = 100
    default = 25


class NothingWeight(Range):
    """Weight (chance) that Nothing is selected for a filler item."""
    display_name = "Filler Weight: Nothing"
    range_start = 0
    range_end = 100
    default = 60


@dataclass
class ArchipeladokuOptions(PerGameCommonOptions):
    block_size: BlockSize
    boards_per_cluster: BoardsPerCluster
    number_of_boards: NumberOfBoards
    difficulty: Difficulty
    progression: Progression
    location_scouting: LocationScouting
    solve_selected_cell_weight: SolveSelectedCellWeight
    solve_random_cell_weight: SolveRandomCellWeight
    remove_random_candidate_weight: RemoveRandomCandidateWeight
    nothing_weight: NothingWeight
