INTERFACE LFSDirHandle;

IMPORT File;


TYPE
  DH = File.DirHandle BRANDED OBJECT
    
  OVERRIDES
    read  := ReadDir;
    close := CloseDir;
  END;


END LFSDirHandle.i3.

