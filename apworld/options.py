from dataclasses import dataclass
import Options
from Options import OptionGroup


class BlockSize(Options.Choice):
    """The size of a single block (and the width/height of each board). A standard Sudoku is 9.
    Smaller sizes are easier, larger sizes are more difficult.
    """
    display_name = "Block Size"
    option_4 = 4
    option_6 = 6
    option_8 = 8
    option_9 = 9
    option_12 = 12
    option_16 = 16
    default = 9


class BoardsPerCluster(Options.Choice):
    """How many boards to put in each cluster of overlapping boards.
    1 creates individual boards without clusters, 100 puts all boards into a
    single big cluster.
    """
    display_name = "Boards per Cluster"
    option_1 = 1
    option_5 = 5
    option_8 = 8
    option_13 = 13
    option_100 = 100
    default = 5


class NumberOfBoards(Options.Range):
    """How many boards to generate. Maximum depend on block size:
    - 4-9: 100 boards
    - 12: 64 boards
    - 16: 36 boards
    """
    display_name = "Number of Boards"
    range_start = 3
    range_end = 100
    default = 5


class Difficulty(Options.Choice):
    """The overall difficulty level. Solving techniques required:
    - Beginner: Naked/hidden singles.
    - Easy: Pointing pairs, box line reduction.
    - Medium: Naked pairs/triples.
    - Hard: Hidden pairs/triples.
    - Very Hard: X-Wing, Swordfish, Y-Wing.
    """
    display_name = "Difficulty"
    option_beginner = 1
    option_easy = 2
    option_medium = 3
    option_hard = 4
    option_very_hard = 5
    default = 2


class Progression(Options.Choice):
    """How blocks are unlocked during the game.
    - Fixed: Blocks are unlocked by progressive block items in a fixed order. Smoother progression.
    - Shuffled: Blocks are unlocked by specific block items. More chaotic progression.
    """
    display_name = "Block Unlocks"
    option_fixed = "fixed"
    option_shuffled = "shuffled"
    default = "shuffled"


class BundleSize(Options.Range):
    """How many blocks a single progression item unlocks at once.
    - A value of 1 disables bundling.
    - The value is clamped to the block size, so a bundle never unlocks more than a whole
      board's worth of blocks.
    - For Fixed progression each "Progressive Block" is worth this many unlocks.
    - For Shuffled progression each bundle is a "Block Bundle" item unlocking this many blocks.
    """
    display_name = "Bundle Size"
    range_start = 1
    range_end = 16
    default = 1


class AdditionalStartingBlocks(Options.Range):
    """How many additional blocks beyond the regular starting board that start unlocked.
    This removes that many blocks from the item pool, replacing them with filler items.
    """
    display_name = "Additional Starting Blocks"
    range_start = 0
    range_end = 1000
    default = 0


class DuplicateProgression(Options.Range):
    """Percent of progression items that should be duplicated.
    - For Fixed progression higher values may lead to progression being too fast and as such a
      lower value is recommended.
    - For Shuffled progression this can safely be set to 100% to make progression a bit faster.
      The blocks to duplicate are chosen randomly if not 100%, though each block can only be duplicated once.
    """
    display_name = "Duplicate Progression Items"
    range_start = 0
    range_end = 100
    default = 0


class DisabledLocations(Options.OptionSet):
    """Location types to remove entirely, reducing the number of locations and filler items. Valid
    values: blocks, rows, columns, boards. If disabling the requested types would leave too few
    locations for the progression items, some types are automatically re-enabled (a warning is
    logged). Higher bundle sizes allow disabling more, since they reduce the number of progression
    items.
    """
    display_name = "Disabled Locations"
    valid_keys = {"blocks", "rows", "columns", "boards"}


class LocationScouting(Options.Choice):
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


class SolveSelectedCellRatio(Options.Range):
    """Ratio of Solve Selected Cell filler items to number of boards, in percent."""
    display_name = "Filler Ratio: Solve Selected Cell"
    range_start = 0
    range_end = 5000
    default = 100


class SolveRandomCellRatio(Options.Range):
    """Ratio of Solve Random Cell filler items to number of boards, in percent."""
    display_name = "Filler Ratio: Solve Random Cell"
    range_start = 0
    range_end = 5000
    default = 150


class RemoveRandomCandidateRatio(Options.Range):
    """Ratio of Remove Random Candidate filler items to number of boards, in percent."""
    display_name = "Filler Ratio: Remove Random Candidate"
    range_start = 0
    range_end = 5000
    default = 300


class EmojiTrapRatio(Options.Range):
    """Ratio of Emoji Trap filler items to number of boards, in percent.
    When received your numbers will be replaced with emojis for a time.
    """
    display_name = "Filler Ratio: Emoji Trap"
    range_start = 0
    range_end = 5000
    default = 20


class DiscoTrapRatio(Options.Range):
    """Ratio of Disco Trap filler items to number of boards, in percent.
    When received all cells will shift colors for a time.
    """
    display_name = "Filler Ratio: Disco Trap"
    range_start = 0
    range_end = 5000
    default = 20


class TunnelVisionTrapRatio(Options.Range):
    """Ratio of Tunnel Vision Trap filler items to number of boards, in percent.
    When received you will only be able to see a small area of the board for a time.
    """
    display_name = "Filler Ratio: Tunnel Vision Trap"
    range_start = 0
    range_end = 5000
    default = 20


class PreFillNothingsPercent(Options.Range):
    """Percentage of Nothing items that should be pre-filled, forcing them to be placed in
    an Archipeladoku game and thus excluding them from other games.
    Caution: This reduces the number of filler items in the item pool. Having few fillers can lead
    to increased generation times or even generation failures. As long as you don't remove other
    filler items this shouldn't be an issue though, even at 100%.
    """
    display_name = "Pre-fill Nothings Percentage"
    range_start = 0
    range_end = 100
    default = 100


class DeathLink(Options.DeathLink):
    """Enable Death Link. When a player with death link enabled dies all other players that also
    enabled it die as well. Archipeladoku can only receive death links, not send them. When a death
    link is received all non-given numbers will be cleared. This can also be toggled in the client
    from the Debug menu.
    """


option_groups = [
    OptionGroup(
        "Board Options",
        [
            BlockSize,
            BoardsPerCluster,
            NumberOfBoards,
            Difficulty,
            Progression,
            BundleSize,
            AdditionalStartingBlocks,
            DuplicateProgression,
            DisabledLocations,
            LocationScouting,
        ],
    ),
    OptionGroup(
        "Filler Options",
        [
            SolveSelectedCellRatio,
            SolveRandomCellRatio,
            RemoveRandomCandidateRatio,
            DiscoTrapRatio,
            EmojiTrapRatio,
            TunnelVisionTrapRatio,
            PreFillNothingsPercent,
        ],
    ),
]


@dataclass
class ArchipeladokuOptions(Options.PerGameCommonOptions):
    block_size: BlockSize
    boards_per_cluster: BoardsPerCluster
    number_of_boards: NumberOfBoards
    difficulty: Difficulty
    progression: Progression
    bundle_size: BundleSize
    additional_starting_blocks: AdditionalStartingBlocks
    duplicate_progression: DuplicateProgression
    disabled_locations: DisabledLocations
    location_scouting: LocationScouting
    solve_selected_cell_ratio: SolveSelectedCellRatio
    solve_random_cell_ratio: SolveRandomCellRatio
    remove_random_candidate_ratio: RemoveRandomCandidateRatio
    emoji_trap_ratio: EmojiTrapRatio
    disco_trap_ratio: DiscoTrapRatio
    tunnel_vision_trap_ratio: TunnelVisionTrapRatio
    pre_fill_nothings_percent: PreFillNothingsPercent
    death_link: DeathLink
    start_inventory_from_pool: Options.StartInventoryPool
