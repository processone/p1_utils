-record(file_q, {len = 0 :: non_neg_integer(),
		 fd       :: file:fd(),
		 path     :: filename:filename(),
		 head = 0 :: non_neg_integer(),
		 tail = 0 :: non_neg_integer()}).

-define(qlen(Q), element(2, Q)).
