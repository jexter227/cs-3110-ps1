open Assertions
open Ps1

TEST_UNIT "test is_mon_inc 1" = assert_true (is_mon_inc [1;2;3;6;9])
TEST_UNIT "test is_mon_inc 2" = assert_true (not (is_mon_inc [1;3;5;7;5;9]))
TEST_UNIT "test is_mon_inc 3" = assert_true (is_mon_inc [1;1;2;3;4;4])
TEST_UNIT "test is_mon_inc 4" = assert_true (is_mon_inc [])
TEST_UNIT "test is_mon_inc 5" = assert_true (is_mon_inc [1])

(*TEST_UNIT "test is_unimodal 1" = assert_true (is_unimodal [1;2;3;6;9;5;4])
	TEST_UNIT "test is_unimodal 2" = assert_true (!is_unimodal)
*) 

TEST_UNIT "test is_unimodal 1" = assert_true (is_unimodal [1;2;3;6;9;5;4])
TEST_UNIT "test is_unimodal 2" = assert_true (not (is_unimodal [1;3;5;7;5;6]))
TEST_UNIT "test is_unimodal 3" = assert_true (is_unimodal [1;1;2;3;4;4;3;2;2;-1])
TEST_UNIT "test is_unimodal 4" = assert_true (is_unimodal [])
TEST_UNIT "test is_unimodal 5" = assert_true (is_unimodal [1;1;1])
TEST_UNIT "test is_unimodal 6" = assert_true (is_unimodal [1;2])
TEST_UNIT "test is_unimodal 7" = assert_true (is_unimodal [2;1])

TEST_UNIT "test powerset 1" = assert_true (powerset ([1]) = [[];[1]])
TEST_UNIT "test powerset 2" = assert_true (List.length(powerset ([1;2])) = 4)
TEST_UNIT "test powerset 3" = assert_true (List.length(powerset ([1;2;3])) = 8)
TEST_UNIT "test powerset 4" = assert_true (powerset([]) = [[]])

TEST_UNIT "test rev_int 1" = assert_true (rev_int(1)=1)
TEST_UNIT "test rev_int 2" = assert_true (rev_int(0)=0)
TEST_UNIT "test rev_int 3" = assert_true (rev_int(-1)=(-1))
TEST_UNIT "test rev_int 4" = assert_true (rev_int(-123)=(-321))
TEST_UNIT "test rev_int 5" = assert_true (rev_int(123)=321)
TEST_UNIT "test rev_int 6" = assert_true (rev_int(10)=1)
TEST_UNIT "test rev_int 7" = assert_true (rev_int(-10)=(-1))

TEST_UNIT "test unflatten 1" = assert_true (unflatten (-1) [1;2;3]=None)
TEST_UNIT "test unflatten 2" = assert_true (unflatten (0) [1;2;3]=None)
TEST_UNIT "test unflatten 3" = assert_true (unflatten(1) [1;2;3]=
	Some[[1];[2];[3]])
TEST_UNIT "test unflatten 4" = assert_true (unflatten(4) [1;2;3]=Some [[1;2;3]])
TEST_UNIT "test unflatten 5" = assert_true (unflatten 2 [1;2;3]=
	Some [[1;2];[3]])
TEST_UNIT "test unflatten 6" = assert_true (unflatten 2 ["5";"2";"3"]=
	Some [["5";"2"];["3"]])
TEST_UNIT "test unflatten 7" = assert_true (unflatten 2 []=Some [])

TEST_UNIT "test int_of_roman 1" = assert_true (int_of_roman [I]=1)
TEST_UNIT "test int_of_roman 2" = assert_true (int_of_roman [M;C;M;L;I;V]=1954)
TEST_UNIT "test int_of_roman 3" = assert_true (int_of_roman [M;M;X;I;V]=2014)
TEST_UNIT "test int_of_roman 4" = assert_true (int_of_roman [I;I;I]=3)
TEST_UNIT "test int_of_roman 5" = assert_true (int_of_roman [X;L;I;I]=42)
TEST_UNIT "test int_of_roman 6" = assert_true (int_of_roman [M;C;M;X;C;I;X]=1999)
TEST_UNIT "test int_of_roman 7" = assert_true (int_of_roman []=0)


let () = Pa_ounit_lib.Runtime.summarize()