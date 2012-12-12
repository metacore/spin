INTERFACE Fetch;

IMPORT Error;
PROCEDURE GetFile(hostname: TEXT; filename: TEXT) : REF ARRAY OF CHAR
	RAISES {Error.E};

END Fetch.

