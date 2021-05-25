

   MEMBER('Krojacnica.clw')                                ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABDROPS.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('KROJACNICA002.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('KROJACNICA001.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeKrojacnica PROCEDURE 

ActionMessage        CSTRING(40)                           !
FDB5::View:FileDrop  VIEW(MJESTO)
                       PROJECT(MJE:Naziv_mjesta)
                       PROJECT(MJE:Postanski_broj)
                     END
Queue:FileDrop       QUEUE                            !Queue declaration for browse/combo box using ?MJE:Naziv_mjesta
MJE:Naziv_mjesta       LIKE(MJE:Naziv_mjesta)         !List box control field - type derived from field
MJE:Postanski_broj     LIKE(MJE:Postanski_broj)       !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::KROJ:Record LIKE(KROJ:RECORD),THREAD
FormWindow           WINDOW('Azuriranje krojacnica...'),AT(,,213,116),CENTERED,CENTER,ICON('DIZAJN\Pocetni p' & |
  'rozor\tailor-shop.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-' & |
  'with-sewing-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Sifra krojacnice:'),AT(8,8),USE(?KROJ:Sifra_krojacnice:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(64,8,60,10),USE(KROJ:Sifra_krojacnice),RIGHT(1),REQ
                       PROMPT('Naziv krojacnice:'),AT(8,28),USE(?KROJ:Naziv_krojacnice:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(68,28,60,10),USE(KROJ:Naziv_krojacnice),CAP,REQ
                       PROMPT('Adresa:'),AT(8,50),USE(?KROJ:Adresa:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(36,50,60,10),USE(KROJ:Adresa),CAP,REQ
                       PROMPT('Postanski broj:'),AT(8,70),USE(?KROJ:Postanski_broj:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@P## ###P),AT(64,70,60,10),USE(KROJ:Postanski_broj)
                       LIST,AT(128,70,,10),USE(MJE:Naziv_mjesta),DROP(5),FORMAT('120C|M~Naziv mjesta~@s30@'),FROM(Queue:FileDrop)
                       BUTTON('Spremi'),AT(8,98,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(53,98,40,12),USE(?Cancel)
                       STRING(@S40),AT(98,98,68),USE(ActionMessage),COLOR(00E0FFFFh)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
FDB5                 CLASS(FileDropClass)                  ! File drop manager
Q                      &Queue:FileDrop                !Reference to display queue
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeKrojacnica')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?KROJ:Sifra_krojacnice:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(KROJ:Record,History::KROJ:Record)
  SELF.AddHistoryField(?KROJ:Sifra_krojacnice,1)
  SELF.AddHistoryField(?KROJ:Naziv_krojacnice,2)
  SELF.AddHistoryField(?KROJ:Adresa,3)
  SELF.AddHistoryField(?KROJ:Postanski_broj,4)
  SELF.AddUpdateFile(Access:KROJACNICA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:KROJACNICA.SetOpenRelated()
  Relate:KROJACNICA.Open                                   ! File KROJACNICA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:KROJACNICA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeKrojacnica',FormWindow)          ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  FDB5.Init(?MJE:Naziv_mjesta,Queue:FileDrop.ViewPosition,FDB5::View:FileDrop,Queue:FileDrop,Relate:MJESTO,ThisWindow)
  FDB5.Q &= Queue:FileDrop
  FDB5.AddSortOrder(MJE:PK_Mjesto_PostBr)
  FDB5.AddField(MJE:Naziv_mjesta,FDB5.Q.MJE:Naziv_mjesta) !List box control field - type derived from field
  FDB5.AddField(MJE:Postanski_broj,FDB5.Q.MJE:Postanski_broj) !Primary key field - type derived from field
  FDB5.AddUpdateField(MJE:Postanski_broj,KROJ:Postanski_broj)
  ThisWindow.AddItem(FDB5.WindowComponent)
  FDB5.DefaultFill = 0
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KROJACNICA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeKrojacnica',FormWindow)       ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PopisMjesta
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?KROJ:Postanski_broj
      IF Access:KROJACNICA.TryValidateField(4)             ! Attempt to validate KROJ:Postanski_broj in KROJACNICA
        SELECT(?KROJ:Postanski_broj)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?KROJ:Postanski_broj
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?KROJ:Postanski_broj{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?KROJ:Postanski_broj
      MJE:Postanski_broj = KROJ:Postanski_broj
      IF Access:MJESTO.TryFetch(MJE:PK_Mjesto_PostBr)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          KROJ:Postanski_broj = MJE:Postanski_broj
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeCUsluga PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::CJE:Record  LIKE(CJE:RECORD),THREAD
FormWindow           WINDOW('Azuriranje cjenika usluga...'),AT(,,189,92),CENTERED,CENTER,ICON('DIZAJN\Pocetn' & |
  'i prozor\list.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with' & |
  '-sewing-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Sifra usluge:'),AT(5,7),USE(?CJE:Sifra_usluge:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(46,8,60,10),USE(CJE:Sifra_usluge),RIGHT(1),REQ
                       PROMPT('Naziv usluge:'),AT(5,26),USE(?CJE:Naziv_usluge:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(50,26,60,10),USE(CJE:Naziv_usluge),CAP,REQ
                       PROMPT('Cijena usluge:'),AT(5,48),USE(?CJE:Cijena_usluge:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@N7.2),AT(52,48,60,10),USE(CJE:Cijena_usluge),DECIMAL(12),REQ
                       BUTTON('Spremi'),AT(4,70,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(49,70,40,12),USE(?Cancel)
                       STRING(@S40),AT(94,70,84),USE(ActionMessage)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeCUsluga')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?CJE:Sifra_usluge:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(CJE:Record,History::CJE:Record)
  SELF.AddHistoryField(?CJE:Sifra_usluge,1)
  SELF.AddHistoryField(?CJE:Naziv_usluge,2)
  SELF.AddHistoryField(?CJE:Cijena_usluge,3)
  SELF.AddUpdateFile(Access:CJENIK_USLUGA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:CJENIK_USLUGA.Open                                ! File CJENIK_USLUGA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:CJENIK_USLUGA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeCUsluga',FormWindow)             ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:CJENIK_USLUGA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeCUsluga',FormWindow)          ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeKrojaca PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::KRO:Record  LIKE(KRO:RECORD),THREAD
FormWindow           WINDOW('Azuriranje krojaca...'),AT(,,218,164),CENTERED,CENTER,ICON('DIZAJN\Pocetni proz' & |
  'or\fashion-designer.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-patter' & |
  'n-with-sewing-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Sifra krojaca:'),AT(6,8),USE(?KRO:Sifra_krojaca:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(58,8,60,10),USE(KRO:Sifra_krojaca),RIGHT(1),REQ
                       PROMPT('Sifra krojacnice:'),AT(6,26),USE(?KRO:Sifra_krojacnice:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(66,26,60,10),USE(KRO:Sifra_krojacnice),RIGHT(1)
                       BUTTON('...'),AT(130,24,12,12),USE(?CallLookup)
                       PROMPT('Ime krojaca:'),AT(6,45),USE(?KRO:Ime_krojaca:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(52,45,60,10),USE(KRO:Ime_krojaca),CAP,REQ
                       PROMPT('Prezime krojaca:'),AT(6,63),USE(?KRO:Prezime_krojaca:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(66,63,60,10),USE(KRO:Prezime_krojaca),CAP,REQ
                       PROMPT('Datum rodjenja:'),AT(6,83),USE(?KRO:Datum_rodjenja:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@D6),AT(64,83,60,10),USE(KRO:Datum_rodjenja),REQ
                       BUTTON('...'),AT(130,81,12,12),USE(?Calendar)
                       OPTION('Spol krojaca:'),AT(155,14,,46),USE(KRO:Spol_krojaca),BOXED,COLOR(00E0FFFFh)
                         RADIO('Zensko'),AT(165,26),USE(?OPTION1:RADIO1)
                         RADIO('Musko'),AT(165,44),USE(?OPTION1:RADIO2)
                       END
                       PROMPT('Strucna sprema:'),AT(6,99),USE(?KRO:Strucna_sprema:Prompt),COLOR(00E0FFFFh)
                       LIST,AT(66,100,90,9),USE(KRO:Strucna_sprema,,?KRO:Strucna_sprema:2),LEFT,DROP(2),FROM('Niza struc' & |
  'na sprema|#Niza strucna sprema|Srednja strucna sprema|#Srednja strucna sprema')
                       PROMPT('Iznos place:'),AT(6,119),USE(?KRO:Iznos_place:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@N7.2),AT(52,119,60,10),USE(KRO:Iznos_place),DECIMAL(12),REQ
                       BUTTON('Spremi'),AT(6,144,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(51,144,40,12),USE(?Cancel)
                       STRING(@S40),AT(96,144,66),USE(ActionMessage),COLOR(00E0FFFFh)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
Calendar5            CalendarClass
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeKrojaca')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?KRO:Sifra_krojaca:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(KRO:Record,History::KRO:Record)
  SELF.AddHistoryField(?KRO:Sifra_krojaca,1)
  SELF.AddHistoryField(?KRO:Sifra_krojacnice,8)
  SELF.AddHistoryField(?KRO:Ime_krojaca,2)
  SELF.AddHistoryField(?KRO:Prezime_krojaca,3)
  SELF.AddHistoryField(?KRO:Datum_rodjenja,5)
  SELF.AddHistoryField(?KRO:Spol_krojaca,4)
  SELF.AddHistoryField(?KRO:Strucna_sprema:2,6)
  SELF.AddHistoryField(?KRO:Iznos_place,7)
  SELF.AddUpdateFile(Access:KROJAC)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:KROJAC.SetOpenRelated()
  Relate:KROJAC.Open                                       ! File KROJAC used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:KROJAC
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjeKrojaca',FormWindow)             ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KROJAC.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeKrojaca',FormWindow)          ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  KROJ:Sifra_krojacnice = KRO:Sifra_krojacnice             ! Assign linking field value
  Access:KROJACNICA.Fetch(KROJ:PK_Krojacnica_SifraKrojacnice)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    PopisKrojacnica
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?KRO:Sifra_krojacnice
      IF Access:KROJAC.TryValidateField(8)                 ! Attempt to validate KRO:Sifra_krojacnice in KROJAC
        SELECT(?KRO:Sifra_krojacnice)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?KRO:Sifra_krojacnice
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?KRO:Sifra_krojacnice{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?CallLookup
      ThisWindow.Update()
      KROJ:Sifra_krojacnice = KRO:Sifra_krojacnice
      IF SELF.Run(1,SelectRecord) = RequestCompleted       ! Call lookup procedure and verify RequestCompleted
        KRO:Sifra_krojacnice = KROJ:Sifra_krojacnice
      END
      ThisWindow.Reset(1)
    OF ?Calendar
      ThisWindow.Update()
      Calendar5.SelectOnClose = True
      Calendar5.Ask('Select a Date',KRO:Datum_rodjenja)
      IF Calendar5.Response = RequestCompleted THEN
      KRO:Datum_rodjenja=Calendar5.SelectedDate
      DISPLAY(?KRO:Datum_rodjenja)
      END
      ThisWindow.Reset(True)
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?KRO:Sifra_krojacnice
      KROJ:Sifra_krojacnice = KRO:Sifra_krojacnice
      IF Access:KROJACNICA.TryFetch(KROJ:PK_Krojacnica_SifraKrojacnice)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          KRO:Sifra_krojacnice = KROJ:Sifra_krojacnice
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeKlijenata PROCEDURE 

ActionMessage        CSTRING(40)                           !
BRW5::View:Browse    VIEW(POPRAVLJA)
                       PROJECT(POP:Sifra_usluge)
                       PROJECT(POP:Sifra_krojaca)
                       PROJECT(POP:Broj_popravaka)
                       PROJECT(POP:Cijena_popravaka)
                       PROJECT(POP:Vrijeme)
                       PROJECT(POP:Datum)
                       PROJECT(POP:Sifra_klijenta)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
POP:Sifra_usluge       LIKE(POP:Sifra_usluge)         !List box control field - type derived from field
POP:Sifra_krojaca      LIKE(POP:Sifra_krojaca)        !List box control field - type derived from field
POP:Broj_popravaka     LIKE(POP:Broj_popravaka)       !List box control field - type derived from field
POP:Cijena_popravaka   LIKE(POP:Cijena_popravaka)     !List box control field - type derived from field
POP:Vrijeme            LIKE(POP:Vrijeme)              !List box control field - type derived from field
POP:Datum              LIKE(POP:Datum)                !List box control field - type derived from field
POP:Sifra_klijenta     LIKE(POP:Sifra_klijenta)       !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BRW7::View:Browse    VIEW(SIVA)
                       PROJECT(SIV:Sifra_krojaca)
                       PROJECT(SIV:Sifra_tkanine)
                       PROJECT(SIV:Cijena_sivanja)
                       PROJECT(SIV:Iznos_sivanja_ukupno)
                       PROJECT(SIV:Popust)
                       PROJECT(SIV:Iznos_popusta)
                       PROJECT(SIV:Naziv_proizvoda)
                       PROJECT(SIV:Velicina)
                       PROJECT(SIV:Napomena)
                       PROJECT(SIV:Vrijeme)
                       PROJECT(SIV:Datum)
                       PROJECT(SIV:Sifra_klijenta)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?List:2
SIV:Sifra_krojaca      LIKE(SIV:Sifra_krojaca)        !List box control field - type derived from field
SIV:Sifra_tkanine      LIKE(SIV:Sifra_tkanine)        !List box control field - type derived from field
SIV:Cijena_sivanja     LIKE(SIV:Cijena_sivanja)       !List box control field - type derived from field
SIV:Iznos_sivanja_ukupno LIKE(SIV:Iznos_sivanja_ukupno) !List box control field - type derived from field
SIV:Popust             LIKE(SIV:Popust)               !List box control field - type derived from field
SIV:Iznos_popusta      LIKE(SIV:Iznos_popusta)        !List box control field - type derived from field
SIV:Naziv_proizvoda    LIKE(SIV:Naziv_proizvoda)      !List box control field - type derived from field
SIV:Velicina           LIKE(SIV:Velicina)             !List box control field - type derived from field
SIV:Napomena           LIKE(SIV:Napomena)             !List box control field - type derived from field
SIV:Vrijeme            LIKE(SIV:Vrijeme)              !List box control field - type derived from field
SIV:Datum              LIKE(SIV:Datum)                !List box control field - type derived from field
SIV:Sifra_klijenta     LIKE(SIV:Sifra_klijenta)       !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::KLI:Record  LIKE(KLI:RECORD),THREAD
FormWindow           WINDOW('Azuriranje klijenata...'),AT(,,340,380),CENTERED,CENTER,ICON('DIZAJN\Pocetni pr' & |
  'ozor\network.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with-' & |
  'sewing-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Sifra klijenta:'),AT(4,6),USE(?KLI:Sifra_klijenta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(52,6,60,10),USE(KLI:Sifra_klijenta),RIGHT(1),REQ
                       PROMPT('OIB klijenta:'),AT(120,6),USE(?KLI:OIB_klijenta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@P###########P),AT(164,6,60,10),USE(KLI:OIB_klijenta),REQ
                       PROMPT('Ime klijenta:'),AT(4,28),USE(?KLI:Ime_klijenta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(52,28,60,10),USE(KLI:Ime_klijenta),CAP,REQ
                       PROMPT('Prezime klijenta:'),AT(120,28),USE(?KLI:Prezime_klijenta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(182,28,60,10),USE(KLI:Prezime_klijenta),CAP,REQ
                       STRING('Spol klijenta:'),AT(3,48),USE(?STRING1),COLOR(00E0FFFFh)
                       LIST,AT(52,48,68,9),USE(KLI:Spol_klijenta),CENTER,DROP(2),FROM('Zensko|#Zensko|Musko|#Musko')
                       BUTTON,AT(130,44,16,14),USE(?Save),KEY(CtrlAltEnter),ICON('SAVE.ICO'),TIP('Save Record and Close')
                       STRING('Popis popravlja:'),AT(4,66),USE(?STRING2),FONT(,,,FONT:bold),COLOR(00E0FFFFh)
                       LIST,AT(14,80,318,100),USE(?List),RIGHT(1),HVSCROLL,COLUMN,FORMAT('74C|M~Sifra usluge~@' & |
  'n4@102C|M~Sifra krojaca~@n4@118C|M~Broj popravka~@n4@122C|M~Cijena popravka~@n7.2@13' & |
  '4C|M~Vrijeme~@T4@40C|M~Datum~@D6@'),FROM(Queue:Browse),IMM
                       BUTTON('Unos'),AT(20,185,42,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(70,185,42,12),USE(?Change)
                       BUTTON('Brisanje'),AT(120,185,42,12),USE(?Delete)
                       PROMPT('Puta popravljao:'),AT(204,186),USE(?KLI:Puta_popravljao:Prompt),FONT(,,,FONT:bold), |
  COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(266,186,60,10),USE(KLI:Puta_popravljao),RIGHT(1)
                       STRING('Popis siva:'),AT(4,206),USE(?STRING3),FONT(,,,FONT:bold),COLOR(00E0FFFFh)
                       LIST,AT(14,219,318,100),USE(?List:2),RIGHT(1),HVSCROLL,FORMAT('64C|M~Sifra krojaca~@n4@' & |
  '62C|M~Sifra tkanine~C(1)@n4@65C|M~Cijena sivanja~D(12)@n7.2@87C|M~Iznos sivanja ukup' & |
  'no~@n7.2@92C|M~Popust~@n3@105C|M~Iznos popusta~@n4@120C|M~Naziv proizvoda~@s30@80C|M' & |
  '~Velicina~@s20@353C|M~Napomena~@s101@87C|M~Vrijeme~@T4@40C|M~Datum~@D6@'),FROM(Queue:Browse:1), |
  IMM
                       BUTTON('Unos'),AT(20,328,42,12),USE(?Insert:2)
                       BUTTON('Izmjena'),AT(70,328,42,12),USE(?Change:2)
                       BUTTON('Brisanje'),AT(120,328,42,12),USE(?Delete:2)
                       PROMPT('Ukupno potrosio sivanje:'),AT(175,329),USE(?KLI:Ukupno_potrosio_sivanje:Prompt),FONT(, |
  ,,FONT:bold),COLOR(00E0FFFFh)
                       ENTRY(@N7.2),AT(266,329,60,10),USE(KLI:Ukupno_potrosio_sivanje),DECIMAL(12)
                       BUTTON('Spremi'),AT(4,352,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(48,352,40,12),USE(?Cancel)
                       STRING(@S40),AT(94,352,68),USE(ActionMessage),COLOR(00E0FFFFh)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
BRW5                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetFromView          PROCEDURE(),DERIVED
                     END

BRW5::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW7                 CLASS(BrowseClass)                    ! Browse using ?List:2
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetFromView          PROCEDURE(),DERIVED
SetAlerts              PROCEDURE(),DERIVED
                     END

BRW7::Sort0:Locator  StepLocatorClass                      ! Default Locator
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjeKlijenata')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?KLI:Sifra_klijenta:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(KLI:Record,History::KLI:Record)
  SELF.AddHistoryField(?KLI:Sifra_klijenta,1)
  SELF.AddHistoryField(?KLI:OIB_klijenta,2)
  SELF.AddHistoryField(?KLI:Ime_klijenta,3)
  SELF.AddHistoryField(?KLI:Prezime_klijenta,4)
  SELF.AddHistoryField(?KLI:Spol_klijenta,5)
  SELF.AddHistoryField(?KLI:Puta_popravljao,6)
  SELF.AddHistoryField(?KLI:Ukupno_potrosio_sivanje,7)
  SELF.AddUpdateFile(Access:KLIJENT)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:KLIJENT.SetOpenRelated()
  Relate:KLIJENT.Open                                      ! File KLIJENT used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:KLIJENT
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.SaveControl = ?Save
  SELF.DisableCancelButton = 1
  BRW5.Init(?List,Queue:Browse.ViewPosition,BRW5::View:Browse,Queue:Browse,Relate:POPRAVLJA,SELF) ! Initialize the browse manager
  BRW7.Init(?List:2,Queue:Browse:1.ViewPosition,BRW7::View:Browse,Queue:Browse:1,Relate:SIVA,SELF) ! Initialize the browse manager
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  BRW5.Q &= Queue:Browse
  BRW5.AddSortOrder(,POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum) ! Add the sort order for POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum for sort order 1
  BRW5.AddRange(POP:Sifra_klijenta,Relate:POPRAVLJA,Relate:KLIJENT) ! Add file relationship range limit for sort order 1
  BRW5.AddLocator(BRW5::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW5::Sort0:Locator.Init(,POP:Sifra_usluge,1,BRW5)       ! Initialize the browse locator using  using key: POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum , POP:Sifra_usluge
  BRW5.AddField(POP:Sifra_usluge,BRW5.Q.POP:Sifra_usluge)  ! Field POP:Sifra_usluge is a hot field or requires assignment from browse
  BRW5.AddField(POP:Sifra_krojaca,BRW5.Q.POP:Sifra_krojaca) ! Field POP:Sifra_krojaca is a hot field or requires assignment from browse
  BRW5.AddField(POP:Broj_popravaka,BRW5.Q.POP:Broj_popravaka) ! Field POP:Broj_popravaka is a hot field or requires assignment from browse
  BRW5.AddField(POP:Cijena_popravaka,BRW5.Q.POP:Cijena_popravaka) ! Field POP:Cijena_popravaka is a hot field or requires assignment from browse
  BRW5.AddField(POP:Vrijeme,BRW5.Q.POP:Vrijeme)            ! Field POP:Vrijeme is a hot field or requires assignment from browse
  BRW5.AddField(POP:Datum,BRW5.Q.POP:Datum)                ! Field POP:Datum is a hot field or requires assignment from browse
  BRW5.AddField(POP:Sifra_klijenta,BRW5.Q.POP:Sifra_klijenta) ! Field POP:Sifra_klijenta is a hot field or requires assignment from browse
  BRW7.Q &= Queue:Browse:1
  BRW7.AddSortOrder(,SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum) ! Add the sort order for SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum for sort order 1
  BRW7.AddRange(SIV:Sifra_klijenta,Relate:SIVA,Relate:KLIJENT) ! Add file relationship range limit for sort order 1
  BRW7.AddLocator(BRW7::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW7::Sort0:Locator.Init(,SIV:Sifra_krojaca,1,BRW7)      ! Initialize the browse locator using  using key: SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum , SIV:Sifra_krojaca
  BRW7.AddField(SIV:Sifra_krojaca,BRW7.Q.SIV:Sifra_krojaca) ! Field SIV:Sifra_krojaca is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Sifra_tkanine,BRW7.Q.SIV:Sifra_tkanine) ! Field SIV:Sifra_tkanine is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Cijena_sivanja,BRW7.Q.SIV:Cijena_sivanja) ! Field SIV:Cijena_sivanja is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Iznos_sivanja_ukupno,BRW7.Q.SIV:Iznos_sivanja_ukupno) ! Field SIV:Iznos_sivanja_ukupno is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Popust,BRW7.Q.SIV:Popust)              ! Field SIV:Popust is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Iznos_popusta,BRW7.Q.SIV:Iznos_popusta) ! Field SIV:Iznos_popusta is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Naziv_proizvoda,BRW7.Q.SIV:Naziv_proizvoda) ! Field SIV:Naziv_proizvoda is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Velicina,BRW7.Q.SIV:Velicina)          ! Field SIV:Velicina is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Napomena,BRW7.Q.SIV:Napomena)          ! Field SIV:Napomena is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Vrijeme,BRW7.Q.SIV:Vrijeme)            ! Field SIV:Vrijeme is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Datum,BRW7.Q.SIV:Datum)                ! Field SIV:Datum is a hot field or requires assignment from browse
  BRW7.AddField(SIV:Sifra_klijenta,BRW7.Q.SIV:Sifra_klijenta) ! Field SIV:Sifra_klijenta is a hot field or requires assignment from browse
  INIMgr.Fetch('AzuriranjeKlijenata',FormWindow)           ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  BRW5.AskProcedure = 1                                    ! Will call: AzuriranjePopravlja
  BRW7.AskProcedure = 2                                    ! Will call: AzuriranjSiva
  BRW5.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW7.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KLIJENT.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeKlijenata',FormWindow)        ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      AzuriranjePopravlja
      AzuriranjSiva
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW5.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW5.ResetFromView PROCEDURE

KLI:Puta_popravljao:Cnt LONG                               ! Count variable for browse totals
  CODE
  SETCURSOR(Cursor:Wait)
  Relate:POPRAVLJA.SetQuickScan(1)
  SELF.Reset
  IF SELF.UseMRP
     IF SELF.View{PROP:IPRequestCount} = 0
          SELF.View{PROP:IPRequestCount} = 60
     END
  END
  LOOP
    IF SELF.UseMRP
       IF SELF.View{PROP:IPRequestCount} = 0
            SELF.View{PROP:IPRequestCount} = 60
       END
    END
    CASE SELF.Next()
    OF Level:Notify
      BREAK
    OF Level:Fatal
      SETCURSOR()
      RETURN
    END
    SELF.SetQueueRecord
    KLI:Puta_popravljao:Cnt += 1
  END
  SELF.View{PROP:IPRequestCount} = 0
  KLI:Puta_popravljao = KLI:Puta_popravljao:Cnt
  PARENT.ResetFromView
  Relate:POPRAVLJA.SetQuickScan(0)
  SETCURSOR()


BRW7.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW7.ResetFromView PROCEDURE

KLI:Ukupno_potrosio_sivanje:Sum REAL                       ! Sum variable for browse totals
  CODE
  SETCURSOR(Cursor:Wait)
  Relate:SIVA.SetQuickScan(1)
  SELF.Reset
  IF SELF.UseMRP
     IF SELF.View{PROP:IPRequestCount} = 0
          SELF.View{PROP:IPRequestCount} = 60
     END
  END
  LOOP
    IF SELF.UseMRP
       IF SELF.View{PROP:IPRequestCount} = 0
            SELF.View{PROP:IPRequestCount} = 60
       END
    END
    CASE SELF.Next()
    OF Level:Notify
      BREAK
    OF Level:Fatal
      SETCURSOR()
      RETURN
    END
    SELF.SetQueueRecord
    KLI:Ukupno_potrosio_sivanje:Sum += SIV:Iznos_sivanja_ukupno
  END
  SELF.View{PROP:IPRequestCount} = 0
  KLI:Ukupno_potrosio_sivanje = KLI:Ukupno_potrosio_sivanje:Sum
  PARENT.ResetFromView
  Relate:SIVA.SetQuickScan(0)
  SETCURSOR()


BRW7.SetAlerts PROCEDURE

  CODE
  SELF.EditViaPopup = False
  PARENT.SetAlerts

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjePopravlja PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::POP:Record  LIKE(POP:RECORD),THREAD
FormWindow           WINDOW('Azuriranje popravlja...'),AT(,,219,139),CENTERED,CENTER,ICON('DIZAJN\Pocetni pr' & |
  'ozor\thread.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with-s' & |
  'ewing-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       STRING('Broj popravaka:'),AT(5,8),USE(?STRING1),COLOR(00E0FFFFh)
                       SPIN(@n4),AT(62,8,60,10),USE(POP:Broj_popravaka,,?POP:Broj_popravaka:2)
                       PROMPT('Sifra usluge:'),AT(5,26),USE(?POP:Sifra_usluge:Prompt),FONT(,,,FONT:regular),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(50,26,60,10),USE(POP:Sifra_usluge),RIGHT(1)
                       PROMPT('Cijena popravaka:'),AT(5,44),USE(?POP:Cijena_popravaka:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n7.2),AT(69,44,60,10),USE(POP:Cijena_popravaka),DECIMAL(12),REQ
                       PROMPT('Vrijeme:'),AT(5,64),USE(?POP:Vrijeme:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@T4),AT(34,64,60,10),USE(POP:Vrijeme),REQ
                       PROMPT('Datum:'),AT(5,84),USE(?POP:Datum:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@D6),AT(30,84,60,10),USE(POP:Datum),REQ
                       BUTTON('...'),AT(97,83,12,12),USE(?Calendar)
                       PROMPT('Sifra krojaca:'),AT(5,100),USE(?POP:Sifra_krojaca:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(50,100,60,10),USE(POP:Sifra_krojaca),RIGHT(1)
                       STRING(@s30),AT(120,100,36),USE(KRO:Ime_krojaca),COLOR(00E0FFFFh)
                       STRING(@s30),AT(166,100,46),USE(KRO:Prezime_krojaca),COLOR(00E0FFFFh)
                       BUTTON('Spremi'),AT(5,120,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(50,120,40,12),USE(?Cancel)
                       STRING(@S40),AT(95,120,66),USE(ActionMessage),COLOR(00E0FFFFh)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
Calendar5            CalendarClass
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjePopravlja')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?STRING1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(POP:Record,History::POP:Record)
  SELF.AddHistoryField(?POP:Broj_popravaka:2,4)
  SELF.AddHistoryField(?POP:Sifra_usluge,3)
  SELF.AddHistoryField(?POP:Cijena_popravaka,5)
  SELF.AddHistoryField(?POP:Vrijeme,6)
  SELF.AddHistoryField(?POP:Datum,2)
  SELF.AddHistoryField(?POP:Sifra_krojaca,7)
  SELF.AddUpdateFile(Access:POPRAVLJA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:CJENIK_USLUGA.SetOpenRelated()
  Relate:CJENIK_USLUGA.Open                                ! File CJENIK_USLUGA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:POPRAVLJA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjePopravlja',FormWindow)           ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:CJENIK_USLUGA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjePopravlja',FormWindow)        ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  KRO:Sifra_krojaca = POP:Sifra_krojaca                    ! Assign linking field value
  Access:KROJAC.Fetch(KRO:PK_Krojac_SifraKrojaca)
  CJE:Sifra_usluge = POP:Sifra_usluge                      ! Assign linking field value
  Access:CJENIK_USLUGA.Fetch(CJE:PK_Cjenik_SifraUsluge)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      PopisCUsluga
      PopisKrojaca
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?POP:Sifra_usluge
      IF Access:POPRAVLJA.TryValidateField(3)              ! Attempt to validate POP:Sifra_usluge in POPRAVLJA
        SELECT(?POP:Sifra_usluge)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?POP:Sifra_usluge
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?POP:Sifra_usluge{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?Calendar
      ThisWindow.Update()
      Calendar5.SelectOnClose = True
      Calendar5.Ask('Select a Date',POP:Datum)
      IF Calendar5.Response = RequestCompleted THEN
      POP:Datum=Calendar5.SelectedDate
      DISPLAY(?POP:Datum)
      END
      ThisWindow.Reset(True)
    OF ?POP:Sifra_krojaca
      IF Access:POPRAVLJA.TryValidateField(7)              ! Attempt to validate POP:Sifra_krojaca in POPRAVLJA
        SELECT(?POP:Sifra_krojaca)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?POP:Sifra_krojaca
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?POP:Sifra_krojaca{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?POP:Cijena_popravaka
      POP:Cijena_popravaka=POP:Broj_popravaka*CJE:Cijena_usluge
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?POP:Sifra_usluge
      CJE:Sifra_usluge = POP:Sifra_usluge
      IF Access:CJENIK_USLUGA.TryFetch(CJE:PK_Cjenik_SifraUsluge)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          POP:Sifra_usluge = CJE:Sifra_usluge
        END
      END
      ThisWindow.Reset()
    OF ?POP:Sifra_krojaca
      KRO:Sifra_krojaca = POP:Sifra_krojaca
      IF Access:KROJAC.TryFetch(KRO:PK_Krojac_SifraKrojaca)
        IF SELF.Run(2,SelectRecord) = RequestCompleted
          POP:Sifra_krojaca = KRO:Sifra_krojaca
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjSiva PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::SIV:Record  LIKE(SIV:RECORD),THREAD
FormWindow           WINDOW('Azuriranje siva...'),AT(,,310,155),CENTERED,CENTER,ICON('DIZAJN\Pocetni prozor\' & |
  'needle.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with-sewing' & |
  '-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Cijena sivanja:'),AT(6,8),USE(?SIV:Cijena_sivanja:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n7.2),AT(60,8,60,10),USE(SIV:Cijena_sivanja),DECIMAL(12),REQ
                       CHECK(' Popust:'),AT(126,8),USE(SIV:Popust),VALUE('0.15','0')
                       PROMPT('Iznos popusta:'),AT(184,8),USE(?SIV:Iznos_popusta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(236,8,60,10),USE(SIV:Iznos_popusta),RIGHT(1)
                       PROMPT('Iznos sivanja ukupno:'),AT(7,28),USE(?SIV:Iznos_sivanja_ukupno:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n7.2),AT(84,28,60,10),USE(SIV:Iznos_sivanja_ukupno),DECIMAL(12),REQ
                       PROMPT('Naziv proizvoda:'),AT(155,28),USE(?SIV:Naziv_proizvoda:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(214,28,60,10),USE(SIV:Naziv_proizvoda),CAP,REQ
                       STRING('Velicina:'),AT(6,46),USE(?STRING1),COLOR(00E0FFFFh)
                       LIST,AT(37,46,48,9),USE(SIV:Velicina),CENTER,DROP(7),FROM('S|#S|M|#M|L|#L|XS|#XS|XL|#XL' & |
  '|XXL|#XXL|XXXL|#XXXL')
                       PROMPT('Vrijeme:'),AT(8,68),USE(?SIV:Vrijeme:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@T4),AT(37,68,60,10),USE(SIV:Vrijeme),REQ
                       PROMPT('Datum:'),AT(6,90),USE(?SIV:Datum:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@D6),AT(37,90,60,10),USE(SIV:Datum),REQ
                       BUTTON('...'),AT(104,89,12,12),USE(?Calendar)
                       STRING('Napomena:'),AT(124,68),USE(?STRING2),COLOR(00E0FFFFh)
                       TEXT,AT(163,57,103,42),USE(SIV:Napomena),HVSCROLL
                       PROMPT('Sifra krojaca:'),AT(8,114),USE(?SIV:Sifra_krojaca:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(56,114,60,10),USE(SIV:Sifra_krojaca),RIGHT(1)
                       PROMPT('Sifra tkanine:'),AT(126,114),USE(?SIV:Sifra_tkanine:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(173,114,60,10),USE(SIV:Sifra_tkanine),RIGHT(1)
                       BUTTON('Spremi'),AT(6,136,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(51,136,40,12),USE(?Cancel)
                       STRING(@S40),AT(96,136,78),USE(ActionMessage),COLOR(00E0FFFFh)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
Calendar5            CalendarClass
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'Pregled zapisa'
  OF InsertRecord
    ActionMessage = 'Dodavanje zapisa'
  OF ChangeRecord
    ActionMessage = 'Izmjena zapisa'
  END
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AzuriranjSiva')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?SIV:Cijena_sivanja:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(SIV:Record,History::SIV:Record)
  SELF.AddHistoryField(?SIV:Cijena_sivanja,5)
  SELF.AddHistoryField(?SIV:Popust,6)
  SELF.AddHistoryField(?SIV:Iznos_popusta,7)
  SELF.AddHistoryField(?SIV:Iznos_sivanja_ukupno,8)
  SELF.AddHistoryField(?SIV:Naziv_proizvoda,10)
  SELF.AddHistoryField(?SIV:Velicina,11)
  SELF.AddHistoryField(?SIV:Vrijeme,9)
  SELF.AddHistoryField(?SIV:Datum,4)
  SELF.AddHistoryField(?SIV:Napomena,12)
  SELF.AddHistoryField(?SIV:Sifra_krojaca,3)
  SELF.AddHistoryField(?SIV:Sifra_tkanine,1)
  SELF.AddUpdateFile(Access:SIVA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:KROJAC.SetOpenRelated()
  Relate:KROJAC.Open                                       ! File KROJAC used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:SIVA
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(FormWindow)                                    ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('AzuriranjSiva',FormWindow)                 ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KROJAC.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjSiva',FormWindow)              ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF FormWindow{Prop:AcceptAll} THEN RETURN.
  TKA:Sifra_tkanine = SIV:Sifra_tkanine                    ! Assign linking field value
  Access:TKANINA.Fetch(TKA:PK_Tkanina_SifraTkanine)
  KRO:Sifra_krojaca = SIV:Sifra_krojaca                    ! Assign linking field value
  Access:KROJAC.Fetch(KRO:PK_Krojac_SifraKrojaca)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      PopisKrojaca
      PopisTkanina
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Calendar
      ThisWindow.Update()
      Calendar5.SelectOnClose = True
      Calendar5.Ask('Select a Date',SIV:Datum)
      IF Calendar5.Response = RequestCompleted THEN
      SIV:Datum=Calendar5.SelectedDate
      DISPLAY(?SIV:Datum)
      END
      ThisWindow.Reset(True)
    OF ?SIV:Sifra_krojaca
      IF Access:SIVA.TryValidateField(3)                   ! Attempt to validate SIV:Sifra_krojaca in SIVA
        SELECT(?SIV:Sifra_krojaca)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?SIV:Sifra_krojaca
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?SIV:Sifra_krojaca{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?SIV:Sifra_tkanine
      IF Access:SIVA.TryValidateField(1)                   ! Attempt to validate SIV:Sifra_tkanine in SIVA
        SELECT(?SIV:Sifra_tkanine)
        FormWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?SIV:Sifra_tkanine
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?SIV:Sifra_tkanine{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?SIV:Iznos_popusta
      SIV:Iznos_popusta=SIV:Cijena_sivanja*SIV:Popust
    OF ?SIV:Iznos_sivanja_ukupno
      SIV:Iznos_sivanja_ukupno=SIV:Cijena_sivanja-SIV:Iznos_popusta
    END
  ReturnValue = PARENT.TakeSelected()
    CASE FIELD()
    OF ?SIV:Sifra_krojaca
      KRO:Sifra_krojaca = SIV:Sifra_krojaca
      IF Access:KROJAC.TryFetch(KRO:PK_Krojac_SifraKrojaca)
        IF SELF.Run(1,SelectRecord) = RequestCompleted
          SIV:Sifra_krojaca = KRO:Sifra_krojaca
        END
      END
      ThisWindow.Reset()
    OF ?SIV:Sifra_tkanine
      TKA:Sifra_tkanine = SIV:Sifra_tkanine
      IF Access:TKANINA.TryFetch(TKA:PK_Tkanina_SifraTkanine)
        IF SELF.Run(2,SelectRecord) = RequestCompleted
          SIV:Sifra_tkanine = TKA:Sifra_tkanine
        END
      END
      ThisWindow.Reset()
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Jednostavno izvjesce
!!! </summary>
IspisSvihKrojaca PROCEDURE 

Progress:Thermometer BYTE                                  !
Process:View         VIEW(KROJAC)
                       PROJECT(KRO:Ime_krojaca)
                       PROJECT(KRO:Iznos_place)
                       PROJECT(KRO:Prezime_krojaca)
                       PROJECT(KRO:Sifra_krojaca)
                       PROJECT(KRO:Sifra_krojacnice)
                     END
ReportPageNumber     LONG,AUTO
ProgressWindow       WINDOW('Ucitavanje...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Odustani'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2000,6250,7688),PRE(RPT),PAPER(PAPER:A4),FONT('Arial',10,,FONT:regular,CHARSET:ANSI), |
  THOUS
                       HEADER,AT(1000,1000,6250,1437),USE(?Header)
                         STRING('Popis svih krojaca'),AT(1208,208,2927),USE(?STRING2),FONT(,20,,FONT:bold+FONT:italic)
                         STRING('Datum izvjestaja:'),AT(3604,635),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(4708,635),USE(?ReportDateStamp),TRN
                         IMAGE('DIZAJN\Pocetni prozor\fashion-designer.png'),AT(167,104),USE(?IMAGE1)
                       END
Detail                 DETAIL,AT(0,0,6250,1250),USE(?Detail)
                         STRING(@n4),AT(260,833),USE(KRO:Sifra_krojaca),RIGHT(1)
                         LINE,AT(167,552,5802,0),USE(?LINE1)
                         STRING('Sifra krojaca'),AT(260,208,875),USE(?STRING3),FONT(,,,FONT:bold)
                         STRING('Ime'),AT(1427,208,635),USE(?STRING4),FONT(,,,FONT:bold)
                         STRING('Prezime'),AT(2698,208,927),USE(?STRING5),FONT(,,,FONT:bold)
                         STRING('Iznos place'),AT(3937,208,948),USE(?STRING6),FONT(,,,FONT:bold)
                         STRING('Sifra krojacnice'),AT(4948,208,1125),USE(?STRING7),FONT(,,,FONT:bold)
                         STRING(@s30),AT(1427,833,958),USE(KRO:Ime_krojaca)
                         STRING(@s30),AT(2833,833,1167),USE(KRO:Prezime_krojaca)
                         STRING(@N7.2),AT(4062,833,573),USE(KRO:Iznos_place),DECIMAL(12)
                         STRING(@n4),AT(5260,833),USE(KRO:Sifra_krojacnice),RIGHT(1)
                       END
                       FOOTER,AT(1000,9688,6250,729),USE(?Footer)
                         STRING(@N3),AT(5521,260,677),USE(ReportPageNumber)
                         STRING('Stranica:'),AT(4708,260,646),USE(?STRING1)
                       END
                       FORM,AT(1000,1000,6250,9688),USE(?Form)
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
OpenReport             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepLongClass                         ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('IspisSvihKrojaca')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:KROJAC.SetOpenRelated()
  Relate:KROJAC.Open                                       ! File KROJAC used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisSvihKrojaca',ProgressWindow)          ! Restore window settings from non-volatile store
  ProgressMgr.Init(ScrollSort:AllowNumeric,)
  ThisReport.Init(Process:View, Relate:KROJAC, ?Progress:PctText, Progress:Thermometer, ProgressMgr, KRO:Sifra_krojaca)
  ThisReport.AddSortOrder(KRO:PK_Krojac_SifraKrojaca)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:KROJAC.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KROJAC.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisSvihKrojaca',ProgressWindow)       ! Save window data to non-volatile store
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OpenReport PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.OpenReport()
  IF ReturnValue = Level:Benign
    Report$?ReportPageNumber{PROP:PageNo} = True
  END
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportDateStamp{PROP:Text} = FORMAT(TODAY(),@D17)
  END
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:Detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Izvjesce po gumbu
!!! </summary>
IspisOdabranihKrojaca PROCEDURE 

Progress:Thermometer BYTE                                  !
Process:View         VIEW(KROJAC)
                       PROJECT(KRO:Ime_krojaca)
                       PROJECT(KRO:Iznos_place)
                       PROJECT(KRO:Prezime_krojaca)
                       PROJECT(KRO:Sifra_krojaca)
                       PROJECT(KRO:Sifra_krojacnice)
                     END
ProgressWindow       WINDOW('Ucitavanje...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Odustani'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2000,6250,7688),PRE(RPT),PAPER(PAPER:A4),FONT('Arial',10,,FONT:regular,CHARSET:ANSI), |
  THOUS
                       HEADER,AT(1000,1000,6250,1146),USE(?Header)
                         STRING('Podaci o krojacu'),AT(1146,146,2927),USE(?STRING2),FONT(,20,,FONT:bold+FONT:italic)
                         STRING('Datum izvjestaja:'),AT(3604,573),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(4760,573),USE(?ReportDateStamp),TRN
                         IMAGE('DIZAJN\Pocetni prozor\fashion-designer.png'),AT(135,104),USE(?IMAGE1)
                       END
Detail                 DETAIL,AT(0,0,6250,2406),USE(?Detail)
                         STRING(@n4),AT(1812,229),USE(KRO:Sifra_krojaca),RIGHT(1)
                         STRING('Sifra krojaca:'),AT(271,229,1052),USE(?STRING3),FONT(,,,FONT:bold)
                         STRING('Ime:'),AT(271,635,635),USE(?STRING4),FONT(,,,FONT:bold)
                         STRING('Prezime:'),AT(271,1094,927),USE(?STRING5),FONT(,,,FONT:bold)
                         STRING('Iznos place:'),AT(271,1531,948),USE(?STRING6),FONT(,,,FONT:bold)
                         STRING('Sifra krojacnice:'),AT(271,1906,1333),USE(?STRING7),FONT(,,,FONT:bold)
                         STRING(@s30),AT(1948,635,1812),USE(KRO:Ime_krojaca)
                         STRING(@s30),AT(1948,1094,1729),USE(KRO:Prezime_krojaca)
                         STRING(@N7.2),AT(1812,1531,437),USE(KRO:Iznos_place),DECIMAL(12)
                         STRING(@n4),AT(1812,1906),USE(KRO:Sifra_krojacnice),RIGHT(1)
                         BOX,AT(135,156,5865,2094),USE(?BOX1),COLOR(COLOR:Black),LINEWIDTH(1)
                       END
                       FOOTER,AT(1000,9688,6250,458),USE(?Footer)
                       END
                       FORM,AT(1000,1000,6250,9688),USE(?Form)
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
OpenReport             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepLongClass                         ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('IspisOdabranihKrojaca')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:KROJAC.SetOpenRelated()
  Relate:KROJAC.Open                                       ! File KROJAC used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisOdabranihKrojaca',ProgressWindow)     ! Restore window settings from non-volatile store
  ProgressMgr.Init(ScrollSort:AllowNumeric,)
  ThisReport.Init(Process:View, Relate:KROJAC, ?Progress:PctText, Progress:Thermometer, ProgressMgr, KRO:Sifra_krojaca)
  ThisReport.AddSortOrder(KRO:PK_Krojac_SifraKrojaca)
  ThisReport.AddRange(KRO:Sifra_krojaca)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:KROJAC.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KROJAC.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisOdabranihKrojaca',ProgressWindow)  ! Save window data to non-volatile store
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OpenReport PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.OpenReport()
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportDateStamp{PROP:Text} = FORMAT(TODAY(),@D17)
  END
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:Detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Slozeno izvjesce
!!! </summary>
IspisSvihSivanja PROCEDURE 

Progress:Thermometer BYTE                                  !
LOC:brojac           LONG                                  !
Process:View         VIEW(KLIJENT)
                       PROJECT(KLI:Ime_klijenta)
                       PROJECT(KLI:OIB_klijenta)
                       PROJECT(KLI:Prezime_klijenta)
                       PROJECT(KLI:Sifra_klijenta)
                       JOIN(SIV:PK_Siva_SifraKlijenta_SifraKrojaca_Datum,KLI:Sifra_klijenta),INNER
                         PROJECT(SIV:Datum)
                         PROJECT(SIV:Iznos_sivanja_ukupno)
                         PROJECT(SIV:Naziv_proizvoda)
                         PROJECT(SIV:Sifra_tkanine)
                         PROJECT(SIV:Sifra_krojaca)
                         JOIN(TKA:PK_Tkanina_SifraTkanine,SIV:Sifra_tkanine)
                           PROJECT(TKA:Boja_tkanine)
                           PROJECT(TKA:Naziv_tkanine)
                         END
                         JOIN(KRO:PK_Krojac_SifraKrojaca,SIV:Sifra_krojaca)
                           PROJECT(KRO:Ime_krojaca)
                           PROJECT(KRO:Prezime_krojaca)
                           PROJECT(KRO:Sifra_krojaca)
                         END
                       END
                     END
ReportPageNumber     LONG,AUTO
ProgressWindow       WINDOW('Ucitavanje...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Odustani'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2000,6250,7688),PRE(RPT),PAPER(PAPER:A4),FONT('Arial',10,,FONT:regular,CHARSET:ANSI), |
  THOUS
                       HEADER,AT(1000,1000,6250,1073),USE(?Header)
                         STRING('Popis svih šivanja po klijentima'),AT(1052,250),USE(?STRING1),FONT(,20,,FONT:bold+FONT:italic)
                         STRING('Datum izvjestaja:'),AT(3677,677),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(4781,677),USE(?ReportDateStamp),TRN
                         IMAGE('DIZAJN\Pocetni prozor\needles.png'),AT(115,167,667,531),USE(?IMAGE1)
                       END
breakKlijentSifra      BREAK(KLI:Sifra_klijenta),USE(?BREAK1)
breakSivanjeDatum        BREAK(SIV:Datum),USE(?BREAK2)
                           HEADER,AT(0,0,6250,1812),USE(?GROUPHEADER1)
                             STRING('Klijent:'),AT(229,260),USE(?STRING2),FONT(,,,FONT:bold)
                             STRING('Sifra:'),AT(781,260),USE(?STRING3)
                             STRING('OIB:'),AT(229,521),USE(?STRING4)
                             STRING('Ime:'),AT(229,781),USE(?STRING5)
                             STRING('Prezime:'),AT(229,1042),USE(?STRING6)
                             STRING(@n4),AT(1167,260),USE(KLI:Sifra_klijenta),RIGHT(1)
                             STRING(@P###########P),AT(573,521),USE(KLI:OIB_klijenta)
                             STRING(@s30),AT(562,781,1417),USE(KLI:Ime_klijenta)
                             STRING(@s30),AT(844,1042,1406),USE(KLI:Prezime_klijenta)
                             STRING('Krojac:'),AT(3677,260),USE(?STRING7),FONT(,,,FONT:bold)
                             STRING('Sifra:'),AT(4219,260),USE(?STRING8)
                             STRING('Ime:'),AT(3677,521),USE(?STRING9)
                             STRING('Prezime:'),AT(3677,781),USE(?STRING10)
                             STRING('Datum sivanja'),AT(229,1344),USE(?STRING11)
                             STRING('Naziv tkanine'),AT(1302,1344),USE(?STRING13)
                             LINE,AT(73,1615,5927,0),USE(?LINE1)
                             STRING('Iznos sivanja'),AT(5052,1344),USE(?STRING18)
                             STRING(@n4),AT(4604,260),USE(KRO:Sifra_krojaca),RIGHT(1)
                             STRING(@s30),AT(4010,521,1542),USE(KRO:Ime_krojaca,,?KRO:Ime_krojaca:2)
                             STRING(@s30),AT(4292,781,1729),USE(KRO:Prezime_krojaca)
                             STRING('Boja tkanine'),AT(2573,1344),USE(?STRING14)
                             STRING('Naziv  proizvoda'),AT(3740,1344),USE(?STRING16)
                           END
Detail                     DETAIL,AT(0,0,6250,562),USE(?Detail)
                             STRING(@D6),AT(302,125),USE(SIV:Datum)
                             STRING(@s30),AT(1552,125,854),USE(TKA:Naziv_tkanine)
                             STRING(@n7.2),AT(5052,125),USE(SIV:Iznos_sivanja_ukupno),DECIMAL(12)
                             STRING(@s30),AT(2719,125,1229),USE(TKA:Boja_tkanine)
                             STRING(@s30),AT(4010,125,1250),USE(SIV:Naziv_proizvoda)
                           END
                           FOOTER,AT(0,0,6250,625),USE(?GROUPFOOTER1)
                             LINE,AT(73,52,5906,0),USE(?LINE2)
                             STRING('Ukupno:'),AT(4604,240),USE(?STRING15),FONT(,,,FONT:bold)
                             STRING(@n7.2),AT(5344,240),USE(SIV:Iznos_sivanja_ukupno,,?SIV:Iznos_sivanja_ukupno:2),CENTER, |
  SUM,RESET(breakSivanjeDatum)
                           END
                         END
                         FOOTER,AT(0,0,6250,875),USE(?GROUPFOOTER2),PAGEAFTER(1)
                           STRING('string30'),AT(1167,250),USE(?STRING12),FONT(,,COLOR:White)
                         END
                       END
                       FOOTER,AT(1000,9688,6250,490),USE(?Footer)
                         STRING(@N3),AT(5615,219),USE(ReportPageNumber)
                       END
                       FORM,AT(1000,1000,6250,9688),USE(?Form)
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
OpenReport             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepLongClass                         ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('IspisSvihSivanja')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:KLIJENT.Open                                      ! File KLIJENT used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('IspisSvihSivanja',ProgressWindow)          ! Restore window settings from non-volatile store
  ProgressMgr.Init(ScrollSort:AllowNumeric,)
  ThisReport.Init(Process:View, Relate:KLIJENT, ?Progress:PctText, Progress:Thermometer, ProgressMgr, KLI:Sifra_klijenta)
  ThisReport.AddSortOrder(KLI:PK_Klijent_SifraKlijenta)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:KLIJENT.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KLIJENT.Close
  END
  IF SELF.Opened
    INIMgr.Update('IspisSvihSivanja',ProgressWindow)       ! Save window data to non-volatile store
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OpenReport PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.OpenReport()
  IF ReturnValue = Level:Benign
    Report$?ReportPageNumber{PROP:PageNo} = True
  END
  IF ReturnValue = Level:Benign
    SELF.Report $ ?ReportDateStamp{PROP:Text} = FORMAT(TODAY(),@D17)
  END
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:Detail)
  RETURN ReturnValue

