(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

INTERFACE DispArgs;
IMPORT Regress, DispatcherPrivate;

CONST
  TestName = "disp-args";
  TestHelp = "test argument passing through the dispatcher";
  
CONST
  TestList = ARRAY OF Regress.TestDesc{
    Regress.TestDesc{ "args", DoArgs }
  };

  TestText = ARRAY OF TEXT { 
  "----> Sirpa0 <----\n----> Sirpa0 <----\n----> Sirpa0 <----\n----> Sirpa0 <----\n=> OK\n----> Sirpa1 : 111\n----> Sirpa1 : 111\n----> Sirpa1 : 111\n----> Sirpa1 : 111\n=> OK\n----> Sirpa2 : 111 222\n----> Sirpa2 : 111 222\n----> Sirpa2 : 111 222\n----> Sirpa2 : 111 222\n=> OK\n----> Sirpa3 : 111 222 333\n----> Sirpa3 : 111 222 333\n----> Sirpa3 : 111 222 333\n----> Sirpa3 : 111 222 333\n=> OK\n----> Sirpa4 : 111 222 333 444\n----> Sirpa4 : 111 222 333 444\n----> Sirpa4 : 111 222 333 444\n----> Sirpa4 : 111 222 333 444\n=> OK\n----> Sirpa5 : 111 222 333 444 555\n----> Sirpa5 : 111 222 333 444 555\n----> Sirpa5 : 111 222 333 444 555\n----> Sirpa5 : 111 222 333 444 555\n=> OK\n----> Sirpa6 : 111 222 333 444 555 666\n----> Sirpa6 : 111 222 333 444 555 666\n----> Sirpa6 : 111 222 333 444 555 666\n----> Sirpa6 : 111 222 333 444 555 666\n=> OK\n----> Sirpa7 : 111 222 333 444 555 666 777\n----> Sirpa7 : 111 222 333 444 555 666 777\n----> Sirpa7 : 111 222 333 444 555 666 777\n----> Sirpa7 : 111 222 333 444 555 666 777\n=> OK\n----> Sirpa8 : 111 222 333 444 555 666 777 888\n----> Sirpa8 : 111 222 333 444 555 666 777 888\n----> Sirpa8 : 111 222 333 444 555 666 777 888\n----> Sirpa8 : 111 222 333 444 555 666 777 888\n=> OK\n----> Sirpa9 : 111 222 333 444 555 666 777 888 999\n----> Sirpa9 : 111 222 333 444 555 666 777 888 999\n----> Sirpa9 : 111 222 333 444 555 666 777 888 999\n----> Sirpa9 : 111 222 333 444 555 666 777 888 999\n=> OK\n----> Sirpa10 : 111 222 333 444 555 666 777 888 999 101010\n----> Sirpa10 : 111 222 333 444 555 666 777 888 999 101010\n----> Sirpa10 : 111 222 333 444 555 666 777 888 999 101010\n----> Sirpa10 : 111 222 333 444 555 666 777 888 999 101010\n=> OK\n"
  };

CONST
  nIterations = DispatcherPrivate.MaxOptLevel + 1;

PROCEDURE DoArgs (): BOOLEAN;

PROCEDURE Start(i: INTEGER): BOOLEAN;
PROCEDURE End(): BOOLEAN;

END DispArgs.

