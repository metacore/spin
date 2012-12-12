INTERFACE BuildInfo;
PROCEDURE GetInfo  (VAR version       : TEXT;
                    VAR target        : TEXT;
                    VAR buildDate     : TEXT;
                    VAR builder       : TEXT;
                    VAR thisTree      : TEXT );

PROCEDURE GetMountPoint (): TEXT;

PROCEDURE GetHttpServAddr (): TEXT;

PROCEDURE GetMountPad (): TEXT;

PROCEDURE GetHttpPort (): CARDINAL;

PROCEDURE GetFetchMethod (): TEXT;

END BuildInfo.
