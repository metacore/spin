
// parameters for generating design information fields

// First parameters for dates.  Each type of object has
// a distinct date range, to make it easier to control the 
// results of queries that compare dates.
//
// Currently the important relationship is between assembly
// objects and composite parts, since queries #5 and #6
// ask for assemblies that use composite parts with build
// dates later than the date for the assembly.
//
// The overall picture is that composite parts are divided into
// two classes, "old" and "young", such that we have the following
// picture:
//
//  "Old" composite parts   |  assembly object    |  "young" composite    | 
//  have build dates in     |  have build dates   |  parts have build     |
//  this range.             |  in this range.     |  dates in this range. |
//                          |                     |                       |
// t -----------------------|---------------------|-----------------------|->
//
// The constant "YoungCompFrac" determines the fraction of composite 
// parts that are "young" --- about 1/YoungCompFrac of the composite parts 
// are young.  ("About" due to randomness in how the young parts are
// chosen.)


    const int MinModuleDate   = 1000;	// lower bound for module
                                        // buildDate values

    const int MaxModuleDate   = 1999;	// upper bound for module
                                        // buildDate values

    const int MinAssmDate   = 1000;	// lower bound for assembly
                                        // buildDate values

    const int MaxAssmDate   = 1999;	// upper bound for assembly
                                        // buildDate values

    const int MinAtomicDate   = 1000;	// lower bound for atomic part
                                        // buildDate values

    const int MaxAtomicDate   = 1999;	// upper bound for atomic part
                                        // buildDate values

    const int MinOldCompDate   = 0;	// lower bound for "old" composite
                                        // part buildDate values

    const int MaxOldCompDate   = 999;	// upper bound for "old" composite
                                        // part buildDate values

    const int MinYoungCompDate = 2000;  // lower bound for "young" composite
                                        // part buildDate values

    const int MaxYoungCompDate  = 2999;	// upper bound for "young" composite
                                        // part buildDate values

    const int YoungCompFrac = 10;       // 1/YoungCompFrac composite parts
                                        // have "young" build dates (for 
					// queries #5 and #6.)
//  const int TypeSize  = 10;		// type name size (see OO7.ddl)
    const int NumTypes  = 10;		// # different design type names

// parameters for generating AtomicParts and Connections

    const int XYRange = 100000;		// number of x or y values

// parameters for generating CompositeParts and Documents

//  const int TitleSize      = 40;	// document title size (see OO7.ddl)

#define DocumentText "I am the documentation for composite part #%08d.\n"

    const int DocTextLength = 80;       // must be greater than length of
                                        // expanded DocText

#define ManualText "I am the manual for module #%08d.\n"

    const int ManualTextLength = 80;       // must be greater than length of
                                           // expanded ManualText

