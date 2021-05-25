  MEMBER('Krojacnica.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('Krojacnica_BC0.CLW')
Krojacnica_BC0:DctInit             PROCEDURE
Krojacnica_BC0:DctKill             PROCEDURE
Krojacnica_BC0:FilesInit           PROCEDURE
    END
  END

DctInit PROCEDURE
  CODE
  Krojacnica_BC0:DctInit
  Krojacnica_BC0:FilesInit


DctKill PROCEDURE
  CODE
  Krojacnica_BC0:DctKill

