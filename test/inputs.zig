const std = @import("std");

const @"bol_longest_match.l" = @embedFile("./examples_c/bol_longest_match.l");
const @"bol_longest_match.lang" = @embedFile("./examples_c/bol_longest_match.lang");
const @"c99_ansi.l" = @embedFile("./examples_c/c99_ansi.l");
const @"c99_ansi.lang" = @embedFile("./examples_c/c99_ansi.lang");
const @"c_like_syntax.l" = @embedFile("./examples_c/c_like_syntax.l");
const @"c_like_syntax.lang" = @embedFile("./examples_c/c_like_syntax.lang");
const @"easy_input_unput.l" = @embedFile("./examples_c/easy_input_unput.l");
const @"easy_input_unput.lang" = @embedFile("./examples_c/easy_input_unput.lang");
const @"easy_REJECT.l" = @embedFile("./examples_c/easy_REJECT.l");
const @"easy_REJECT.lang" = @embedFile("./examples_c/easy_REJECT.lang");
const @"easy_tc.l" = @embedFile("./examples_c/easy_tc.l");
const @"easy_tc.lang" = @embedFile("./examples_c/easy_tc.lang");
const @"easy_yyless.l" = @embedFile("./examples_c/easy_yyless.l");
const @"easy_yyless.lang" = @embedFile("./examples_c/easy_yyless.lang");
const @"easy_yymore.l" = @embedFile("./examples_c/easy_yymore.l");
const @"easy_yymore.lang" = @embedFile("./examples_c/easy_yymore.lang");
const @"easy_yymore_2.l" = @embedFile("./examples_c/easy_yymore_2.l");
const @"easy_yymore_2.lang" = @embedFile("./examples_c/easy_yymore_2.lang");
const @"extreme_input_unput.l" = @embedFile("./examples_c/extreme_input_unput.l");
const @"extreme_input_unput.lang" = @embedFile("./examples_c/extreme_input_unput.lang");
const @"extreme_tc.l" = @embedFile("./examples_c/extreme_tc.l");
const @"extreme_tc.lang" = @embedFile("./examples_c/extreme_tc.lang");
const @"hard_input_unput.l" = @embedFile("./examples_c/hard_input_unput.l");
const @"hard_input_unput.lang" = @embedFile("./examples_c/hard_input_unput.lang");
const @"hard_input_unput_2.l" = @embedFile("./examples_c/hard_input_unput_2.l");
const @"hard_input_unput_2.lang" = @embedFile("./examples_c/hard_input_unput_2.lang");
const @"hard_tc.l" = @embedFile("./examples_c/hard_tc.l");
const @"hard_tc.lang" = @embedFile("./examples_c/hard_tc.lang");
const @"medium_yymore.l" = @embedFile("./examples_c/medium_yymore.l");
const @"medium_yymore.lang" = @embedFile("./examples_c/medium_yymore.lang");
const @"start_conditions.l" = @embedFile("./examples_c/start_conditions.l");
const @"start_conditions.lang" = @embedFile("./examples_c/start_conditions.lang");
const @"start_conditions_2.l" = @embedFile("./examples_c/start_conditions_2.l");
const @"start_conditions_2.lang" = @embedFile("./examples_c/start_conditions_2.lang");
const @"start_conditions_and_bol.l" = @embedFile("./examples_c/start_conditions_and_bol.l");
const @"start_conditions_and_bol.lang" = @embedFile("./examples_c/start_conditions_and_bol.lang");
const @"start_conditions_and_bol_2.l" = @embedFile("./examples_c/start_conditions_and_bol_2.l");
const @"start_conditions_and_bol_2.lang" = @embedFile("./examples_c/start_conditions_and_bol_2.lang");
const @"wc.l" = @embedFile("./examples_c/wc.l");
const @"wc.lang" = @embedFile("./examples_c/wc.lang");

pub const tests: std.StaticStringMap([]const u8) = .initComptime(&.{
    .{ @"c99_ansi.l", @"c99_ansi.lang" },
    .{ @"bol_longest_match.l", "bol_longest_match.bol_longest_match.lang" },
});
