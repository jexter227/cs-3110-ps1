open Assertions
open ps1.ml

TEST_UNIT "test is_mon_inc 1" = assert_true (is_mon_inc [1;2;3;6;9])
TEST_UNIT "test is_mon_inc 2" = assert_true (not (is_mon_inc [1;3;5;7;5;9])
TEST_UNIT "test is_mon_inc 3" = assert_true (is_mon_inc [1;1;2;3;4;4])
TEST_UNIT "test is_mon_inc 4" = assert_true (is_mon_inc [])
TEST_UNIT "test is_mon_inc 5" = assert_true (is_mon_inc [1])

(* TEST_UNIT "test is_unimodal 1" = assert_true (is_unimodal [1;2;3;6;9;5;4])
	TEST_UNIT "test is_unimodal 2" = assert_true (!is_unimodal)
*) 

TEST_UNIT "test is_unimodal 1" = assert_true (is_unimodal [1;2;3;6;9;5;4])
TEST_UNIT "test is_unimodal 2" = assert_true (not (is_unimodal [1;3;5;7;5;6]) 
TEST_UNIT "test is_unimodal 3" = assert_true (is_unimodal [1;1;2;3;4;4;3;2;2;-1])
TEST_UNIT "test is_unimodal 4" = assert_true (is_unimodal [])
TEST_UNIT "test is_unimodal 5" = assert_true (is_unimodal [1;1;1])
TEST_UNIT "test is_unimodal 6" = assert_true (is_unimodal [1;2])
TEST_UNIT "test is_unimodal 7" = assert_true (is_unimodal [2;1])

let () = Pa_ounit_lib.Runtime.summarize()