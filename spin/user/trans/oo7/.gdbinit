dir ../lib
dir ../rvmbench
dir ../malloc
dir /spin/yasushi/spin/user/trans/src/osf
handle SIGSEGV nostop noprint
b Debugger__Enter
b ThreadPosix__ImpossibleAcquire

