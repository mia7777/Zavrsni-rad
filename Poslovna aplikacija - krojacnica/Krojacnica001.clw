

   MEMBER('Krojacnica.clw')                                ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABEIP.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('BRWEXT.INC'),ONCE

                     MAP
                       INCLUDE('KROJACNICA001.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('KROJACNICA002.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('KROJACNICA003.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Frame
!!! Pocetni prozor aplikacije
!!! </summary>
GlavniIzbornik PROCEDURE 

SplashProcedureThread LONG
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('Krojaènica'),AT(,,327,223),FONT(,,,FONT:bold,CHARSET:DEFAULT),RESIZE,TILED,HVSCROLL, |
  ICON('DIZAJN\Pocetni prozor\14_-Sewing_Thread-_sewing_machine_tailoring_knit_-128.ico'), |
  CURSOR('DIZAJN\Pocetni prozor\mightnight silverwind.cur'),MAX,STATUS(-1,80,120,45),SYSTEM, |
  WALLPAPER('DIZAJN\416e6c6cf8be9a389e5419125eb32548--sewing-cards-white-sewing-machine.jpg'),IMM
                       MENUBAR,USE(?MENUBAR1),FONT(,,,FONT:regular)
                         MENU('Datoteka'),USE(?FileMenu),ICON('DIZAJN\Pocetni prozor\filess.ico')
                           ITEM('P&rint Setup...'),USE(?PrintSetup),MSG('Setup Printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('Uredi'),USE(?EditMenu),ICON('DIZAJN\Pocetni prozor\edittt.ico')
                           ITEM('Cu&t'),USE(?Cut),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),MSG('Paste contents of Windows Clipboard'),STD(STD:Paste)
                         END
                         MENU('Prozor'),USE(?MENU1),ICON('DIZAJN\Pocetni prozor\window.ico'),MSG('Create and Arr' & |
  'ange windows'),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'),STD(STD:ArrangeIcons)
                         END
                         MENU('Pomoc'),USE(?MENU2),ICON('DIZAJN\Pocetni prozor\question.ico'),MSG('Windows Help')
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'),STD(STD:HelpOnHelp)
                         END
                         MENU('POPIS'),USE(?MENU3),ICON('DIZAJN\Pocetni prozor\Elegantthemes-Beautiful-Flat-Magn' & |
  'ifying-glass.ico')
                           ITEM('Krojacnica'),USE(?ITEM1)
                           ITEM('Krojaca'),USE(?ITEM2)
                           ITEM('Tkanina'),USE(?ITEM3)
                           ITEM('Cjenika usluga'),USE(?ITEM4)
                           ITEM('Mjesta'),USE(?ITEM5)
                           ITEM('Klijenata'),USE(?ITEM6)
                         END
                         MENU('ISPIS'),USE(?MENU4),ICON('DIZAJN\Pocetni prozor\printer.ico')
                           ITEM('svih krojaca'),USE(?ISpisSvihKrojaca)
                           ITEM('svih popravaka'),USE(?IspisSvihPopravaka)
                           ITEM('svih sivanja'),USE(?IspisSvihSivanja)
                         END
                       END
                       TOOLBAR,AT(0,0,327,48),USE(?TOOLBAR1),CENTERED,COLOR(008080F0h)
                         BUTTON('KROJAÈNICA'),AT(8,9,47,29),USE(?BUTTON1),FONT(,10,,FONT:bold),ICON(ICON:None),FLAT, |
  TIP('Popis krojaènica')
                         BUTTON('KROJAÈ'),AT(69,9,38,29),USE(?BUTTON2),FONT(,,,FONT:bold),FLAT,TIP('Popis krojaèa')
                         BUTTON('CJENIK USLUGA'),AT(164,10,44,28),USE(?BUTTON3),FLAT,TIP('Popis cjenika usluga')
                         BUTTON('TKANINA'),AT(115,9,41,29),USE(?BUTTON4),FONT(,,,FONT:bold),FLAT,TIP('Popis tkanina')
                         BUTTON('MJESTO'),AT(216,8,39,30),USE(?BUTTON5),FONT(,,,FONT:bold),FLAT,TIP('Popis mjesta')
                         BUTTON('KLIJENT'),AT(265,10,43,26),USE(?BUTTON6),FONT(,,,FONT:bold),FLAT,TIP('Popis klijenata')
                       END
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Menu::MENUBAR1 ROUTINE                                     ! Code for menu items on ?MENUBAR1
Menu::FileMenu ROUTINE                                     ! Code for menu items on ?FileMenu
Menu::EditMenu ROUTINE                                     ! Code for menu items on ?EditMenu
Menu::MENU1 ROUTINE                                        ! Code for menu items on ?MENU1
Menu::MENU2 ROUTINE                                        ! Code for menu items on ?MENU2
Menu::MENU3 ROUTINE                                        ! Code for menu items on ?MENU3
  CASE ACCEPTED()
  OF ?ITEM1
    START(PopisKrojacnica, 50000)
  OF ?ITEM2
    START(PopisKrojaca, 50000)
  OF ?ITEM3
    START(PopisTkanina, 50000)
  OF ?ITEM4
    START(PopisCUsluga, 50000)
  OF ?ITEM5
    START(PopisMjesta, 50000)
  OF ?ITEM6
    START(PopisKlijenata, 50000)
  END
Menu::MENU4 ROUTINE                                        ! Code for menu items on ?MENU4
  CASE ACCEPTED()
  OF ?ISpisSvihKrojaca
    START(IspisSvihKrojaca, 50000)
  OF ?IspisSvihPopravaka
    START(IspisSvihPopravaka, 50000)
  OF ?IspisSvihSivanja
    START(IspisSvihSivanja, 50000)
  END

ThisWindow.Ask PROCEDURE

  CODE
  IF NOT INRANGE(AppFrame{PROP:Timer},1,100)
    AppFrame{PROP:Timer} = 100
  END
    AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D6)
    AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T4)
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('GlavniIzbornik')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('GlavniIzbornik',AppFrame)                  ! Restore window settings from non-volatile store
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('GlavniIzbornik',AppFrame)               ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
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
    CASE ACCEPTED()
    ELSE
      DO Menu::MENUBAR1                                    ! Process menu items on ?MENUBAR1 menu
      DO Menu::FileMenu                                    ! Process menu items on ?FileMenu menu
      DO Menu::EditMenu                                    ! Process menu items on ?EditMenu menu
      DO Menu::MENU1                                       ! Process menu items on ?MENU1 menu
      DO Menu::MENU2                                       ! Process menu items on ?MENU2 menu
      DO Menu::MENU3                                       ! Process menu items on ?MENU3 menu
      DO Menu::MENU4                                       ! Process menu items on ?MENU4 menu
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?BUTTON1
      START(PopisKrojacnica, 50000)
    OF ?BUTTON2
      START(PopisKrojaca, 50000)
    OF ?BUTTON3
      START(PopisCUsluga, 50000)
    OF ?BUTTON4
      START(PopisTkanina, 50000)
    OF ?BUTTON5
      START(PopisMjesta, 50000)
    OF ?BUTTON6
      START(PopisKlijenata, 50000)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      SplashProcedureThread = START(SkocniProzor)          ! Run the splash window procedure
    OF EVENT:Timer
      AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D6)
      AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T4)
    ELSE
      IF SplashProcedureThread
        IF EVENT() = Event:Accepted
          POST(Event:CloseWindow,,SplashProcedureThread)   ! Close the splash window
          SplashPRocedureThread = 0
        END
     END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SkocniProzor PROCEDURE 

window               WINDOW,AT(,,204,112),FONT('Microsoft Sans Serif',8,,FONT:regular),NOFRAME,CENTER,GRAY,MDI
                       IMAGE,AT(68,61),USE(?IMAGE1)
                       PANEL,AT(1,-42,204,154),USE(?PANEL1),BEVEL(6),FILL(008080F0h)
                       PANEL,AT(7,6,191,98),USE(?PANEL2),BEVEL(-2,1),FILL(00E0FFFFh)
                       STRING('Krojacnica'),AT(13,12,182,22),USE(?String2),FONT('Times New Roman',15,,FONT:bold), |
  CENTER,COLOR(00E0FFFFh)
                       PANEL,AT(12,38,182,7),USE(?PANEL3),BEVEL(2,4,9),FILL(00E0FFFFh)
                       STRING('Mia Puriš'),AT(21,61,72,12),USE(?String1),FONT('Times New Roman',12,,FONT:regular+FONT:underline), |
  CENTER,COLOR(00E0FFFFh)
                       IMAGE('DIZAJN\Pocetni prozor\sewing-machine.png'),AT(103,49,67,49),USE(?IMAGE2)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

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
  GlobalErrors.SetProcedureName('SkocniProzor')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?IMAGE1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
  INIMgr.Fetch('SkocniProzor',window)                      ! Restore window settings from non-volatile store
  TARGET{Prop:Timer} = 500                                 ! Close window on timer event, so configure timer
  TARGET{Prop:Alrt,255} = MouseLeft                        ! Alert mouse clicks that will close window
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.Opened
    INIMgr.Update('SkocniProzor',window)                   ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)                            ! Splash window will close on mouse click
      END
    OF EVENT:LoseFocus
        POST(Event:CloseWindow)                            ! Splash window will close when focus is lost
    OF Event:Timer
      POST(Event:CloseWindow)                              ! Splash window will close on event timer
    OF Event:AlertKey
      CASE KEYCODE()                                       ! Splash window will close on mouse click
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisKrojacnica PROCEDURE 

BRW1::View:Browse    VIEW(KROJACNICA)
                       PROJECT(KROJ:Sifra_krojacnice)
                       PROJECT(KROJ:Naziv_krojacnice)
                       PROJECT(KROJ:Adresa)
                       PROJECT(KROJ:Postanski_broj)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
KROJ:Sifra_krojacnice  LIKE(KROJ:Sifra_krojacnice)    !List box control field - type derived from field
KROJ:Naziv_krojacnice  LIKE(KROJ:Naziv_krojacnice)    !List box control field - type derived from field
KROJ:Adresa            LIKE(KROJ:Adresa)              !List box control field - type derived from field
KROJ:Postanski_broj    LIKE(KROJ:Postanski_broj)      !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis krojacnica'),AT(0,0,247,140),CENTERED,ICON('DIZAJN\Pocetni prozor\tailor-shop.ico'), |
  GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('72C|M~Sifra krojacnice~@n4@120C|M~Nazi' & |
  'v krojacnice~@s30@109C|M~Adresa~@s30@40C|M~Postanski broj~@P## ###P@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(145,110,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PopisKrojacnica')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:KROJACNICA.SetOpenRelated()
  Relate:KROJACNICA.Open                                   ! File KROJACNICA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:KROJACNICA,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,KROJ:PK_Krojacnica_SifraKrojacnice)   ! Add the sort order for KROJ:PK_Krojacnica_SifraKrojacnice for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,KROJ:Sifra_krojacnice,1,BRW1)  ! Initialize the browse locator using  using key: KROJ:PK_Krojacnica_SifraKrojacnice , KROJ:Sifra_krojacnice
  BRW1.AddField(KROJ:Sifra_krojacnice,BRW1.Q.KROJ:Sifra_krojacnice) ! Field KROJ:Sifra_krojacnice is a hot field or requires assignment from browse
  BRW1.AddField(KROJ:Naziv_krojacnice,BRW1.Q.KROJ:Naziv_krojacnice) ! Field KROJ:Naziv_krojacnice is a hot field or requires assignment from browse
  BRW1.AddField(KROJ:Adresa,BRW1.Q.KROJ:Adresa)            ! Field KROJ:Adresa is a hot field or requires assignment from browse
  BRW1.AddField(KROJ:Postanski_broj,BRW1.Q.KROJ:Postanski_broj) ! Field KROJ:Postanski_broj is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisKrojacnica',BrowseWindow)             ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeKrojacnica
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PopisKrojacnica',BrowseWindow)          ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeKrojacnica
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisKrojaca PROCEDURE 

BRW1::View:Browse    VIEW(KROJAC)
                       PROJECT(KRO:Sifra_krojacnice)
                       PROJECT(KRO:Sifra_krojaca)
                       PROJECT(KRO:Ime_krojaca)
                       PROJECT(KRO:Prezime_krojaca)
                       PROJECT(KRO:Spol_krojaca)
                       PROJECT(KRO:Datum_rodjenja)
                       PROJECT(KRO:Strucna_sprema)
                       PROJECT(KRO:Iznos_place)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
KRO:Sifra_krojacnice   LIKE(KRO:Sifra_krojacnice)     !List box control field - type derived from field
KRO:Sifra_krojaca      LIKE(KRO:Sifra_krojaca)        !List box control field - type derived from field
KRO:Ime_krojaca        LIKE(KRO:Ime_krojaca)          !List box control field - type derived from field
KRO:Prezime_krojaca    LIKE(KRO:Prezime_krojaca)      !List box control field - type derived from field
KRO:Spol_krojaca       LIKE(KRO:Spol_krojaca)         !List box control field - type derived from field
KRO:Datum_rodjenja     LIKE(KRO:Datum_rodjenja)       !List box control field - type derived from field
KRO:Strucna_sprema     LIKE(KRO:Strucna_sprema)       !List box control field - type derived from field
KRO:Iznos_place        LIKE(KRO:Iznos_place)          !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis krojaca'),AT(0,0,360,140),TILED,ICON('DIZAJN\Pocetni prozor\fashion-designer.ico'), |
  GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(5,5,344,100),USE(?List),HVSCROLL,FORMAT('74C|M~Sifra krojacnice~@n4@69C|M~Sifra' & |
  ' krojaca~@n4@94C|M~Ime krojaca~@s30@106C|M~Prezime krojaca~@s30@80C|M~Spol krojaca~@' & |
  's20@90C|M~Datum rodjenja~@D6@89C|M~Strucna sprema~@s30@28C|M~Iznos place~@N7.2@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(170,110,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(297,110,40,12),USE(?Close)
                       BUTTON('&Print'),AT(237,108),USE(?Print)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PopisKrojaca')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:KROJAC.SetOpenRelated()
  Relate:KROJAC.Open                                       ! File KROJAC used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:KROJAC,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,KRO:PK_Krojac_SifraKrojaca)           ! Add the sort order for KRO:PK_Krojac_SifraKrojaca for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,KRO:Sifra_krojaca,1,BRW1)      ! Initialize the browse locator using  using key: KRO:PK_Krojac_SifraKrojaca , KRO:Sifra_krojaca
  BRW1.AddField(KRO:Sifra_krojacnice,BRW1.Q.KRO:Sifra_krojacnice) ! Field KRO:Sifra_krojacnice is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Sifra_krojaca,BRW1.Q.KRO:Sifra_krojaca) ! Field KRO:Sifra_krojaca is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Ime_krojaca,BRW1.Q.KRO:Ime_krojaca)    ! Field KRO:Ime_krojaca is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Prezime_krojaca,BRW1.Q.KRO:Prezime_krojaca) ! Field KRO:Prezime_krojaca is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Spol_krojaca,BRW1.Q.KRO:Spol_krojaca)  ! Field KRO:Spol_krojaca is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Datum_rodjenja,BRW1.Q.KRO:Datum_rodjenja) ! Field KRO:Datum_rodjenja is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Strucna_sprema,BRW1.Q.KRO:Strucna_sprema) ! Field KRO:Strucna_sprema is a hot field or requires assignment from browse
  BRW1.AddField(KRO:Iznos_place,BRW1.Q.KRO:Iznos_place)    ! Field KRO:Iznos_place is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisKrojaca',BrowseWindow)                ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeKrojaca
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.PrintProcedure = 2
  BRW1.PrintControl = ?Print
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
    INIMgr.Update('PopisKrojaca',BrowseWindow)             ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
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
      AzuriranjeKrojaca
      IspisOdabranihKrojaca
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisTkanina PROCEDURE 

Loc:Pretraga         STRING(20)                            !
BRW1::View:Browse    VIEW(TKANINA)
                       PROJECT(TKA:Sifra_tkanine)
                       PROJECT(TKA:Naziv_tkanine)
                       PROJECT(TKA:Boja_tkanine)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
TKA:Sifra_tkanine      LIKE(TKA:Sifra_tkanine)        !List box control field - type derived from field
TKA:Naziv_tkanine      LIKE(TKA:Naziv_tkanine)        !List box control field - type derived from field
TKA:Boja_tkanine       LIKE(TKA:Boja_tkanine)         !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis tkanina'),AT(0,0,269,180),TILED,ICON('DIZAJN\Pocetni prozor\fabric.ico'),GRAY, |
  MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(14,46,235,100),USE(?List),HVSCROLL,FORMAT('81C|M~Sifra tkanine~@n4@128C|M~Naziv' & |
  ' tkanine~@s30@120C|M~Boja tkanine~@s30@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(14,152,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(60,152,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(104,152,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(154,152,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(210,152,40,12),USE(?Close)
                       SHEET,AT(8,28,250,144),USE(?SHEET1)
                         TAB('po sifri tkanine'),USE(?TAB1)
                         END
                         TAB('po nazivu tkanine'),USE(?TAB2)
                         END
                         TAB('po boji tkanine'),USE(?TAB3)
                         END
                       END
                       PROMPT('Pretraga:'),AT(10,8),USE(?Loc:Pretraga:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s20),AT(46,8,60,10),USE(Loc:Pretraga)
                       BUTTON('Trazi!'),AT(116,7,,11),USE(?BUTTON1)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  EntryLocatorClass                     ! Default Locator
BRW1::Sort1:Locator  EntryLocatorClass                     ! Conditional Locator - CHOICE(?Sheet1)=2
BRW1::Sort2:Locator  EntryLocatorClass                     ! Conditional Locator - CHOICE(?Sheet1)=3

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
  GlobalErrors.SetProcedureName('PopisTkanina')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:TKANINA.Open                                      ! File TKANINA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:TKANINA,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,TKA:SK_Tkanina_NazivTkanine)          ! Add the sort order for TKA:SK_Tkanina_NazivTkanine for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(?Loc:Pretraga,TKA:Naziv_tkanine,1,BRW1) ! Initialize the browse locator using ?Loc:Pretraga using key: TKA:SK_Tkanina_NazivTkanine , TKA:Naziv_tkanine
  BRW1.AddSortOrder(,TKA:SK_Tkanina_BojaTkanine)           ! Add the sort order for TKA:SK_Tkanina_BojaTkanine for sort order 2
  BRW1.AddLocator(BRW1::Sort2:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort2:Locator.Init(?Loc:Pretraga,TKA:Boja_tkanine,1,BRW1) ! Initialize the browse locator using ?Loc:Pretraga using key: TKA:SK_Tkanina_BojaTkanine , TKA:Boja_tkanine
  BRW1.AddSortOrder(,TKA:PK_Tkanina_SifraTkanine)          ! Add the sort order for TKA:PK_Tkanina_SifraTkanine for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(?Loc:Pretraga,TKA:Sifra_tkanine,1,BRW1) ! Initialize the browse locator using ?Loc:Pretraga using key: TKA:PK_Tkanina_SifraTkanine , TKA:Sifra_tkanine
  BRW1.AddField(TKA:Sifra_tkanine,BRW1.Q.TKA:Sifra_tkanine) ! Field TKA:Sifra_tkanine is a hot field or requires assignment from browse
  BRW1.AddField(TKA:Naziv_tkanine,BRW1.Q.TKA:Naziv_tkanine) ! Field TKA:Naziv_tkanine is a hot field or requires assignment from browse
  BRW1.AddField(TKA:Boja_tkanine,BRW1.Q.TKA:Boja_tkanine)  ! Field TKA:Boja_tkanine is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisTkanina',BrowseWindow)                ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeTkanina
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:TKANINA.Close
  END
  IF SELF.Opened
    INIMgr.Update('PopisTkanina',BrowseWindow)             ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeTkanina
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
    CASE ACCEPTED()
    OF ?BUTTON1
      ThisWindow.Reset(1)
    END
  ReturnValue = PARENT.TakeAccepted()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?Sheet1)=2
    RETURN SELF.SetSort(1,Force)
  ELSIF CHOICE(?Sheet1)=3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisCUsluga PROCEDURE 

BRW1::View:Browse    VIEW(CJENIK_USLUGA)
                       PROJECT(CJE:Sifra_usluge)
                       PROJECT(CJE:Naziv_usluge)
                       PROJECT(CJE:Cijena_usluge)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
CJE:Sifra_usluge       LIKE(CJE:Sifra_usluge)         !List box control field - type derived from field
CJE:Naziv_usluge       LIKE(CJE:Naziv_usluge)         !List box control field - type derived from field
CJE:Cijena_usluge      LIKE(CJE:Cijena_usluge)        !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis cjenika usluga'),AT(0,0,247,140),TILED,ICON('DIZAJN\Pocetni prozor\list.ico'), |
  GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('77C|M~Sifra usluge~@n4@120C|M~Naziv us' & |
  'luge~@s30@28C|M~Cijena usluge~@N7.2@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(145,110,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::EIPManager     BrowseEIPManager                      ! Browse EIP Manager for Browse using ?List
EditInPlace::CJE:Sifra_usluge EditEntryClass               ! Edit-in-place class for field CJE:Sifra_usluge
EditInPlace::CJE:Naziv_usluge EditEntryClass               ! Edit-in-place class for field CJE:Naziv_usluge
EditInPlace::CJE:Cijena_usluge EditEntryClass              ! Edit-in-place class for field CJE:Cijena_usluge

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
  GlobalErrors.SetProcedureName('PopisCUsluga')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:CJENIK_USLUGA.Open                                ! File CJENIK_USLUGA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:CJENIK_USLUGA,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,CJE:PK_Cjenik_SifraUsluge)            ! Add the sort order for CJE:PK_Cjenik_SifraUsluge for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,CJE:Sifra_usluge,1,BRW1)       ! Initialize the browse locator using  using key: CJE:PK_Cjenik_SifraUsluge , CJE:Sifra_usluge
  BRW1.AddField(CJE:Sifra_usluge,BRW1.Q.CJE:Sifra_usluge)  ! Field CJE:Sifra_usluge is a hot field or requires assignment from browse
  BRW1.AddField(CJE:Naziv_usluge,BRW1.Q.CJE:Naziv_usluge)  ! Field CJE:Naziv_usluge is a hot field or requires assignment from browse
  BRW1.AddField(CJE:Cijena_usluge,BRW1.Q.CJE:Cijena_usluge) ! Field CJE:Cijena_usluge is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisCUsluga',BrowseWindow)                ! Restore window settings from non-volatile store
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
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
    INIMgr.Update('PopisCUsluga',BrowseWindow)             ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeCUsluga
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  SELF.EIP &= BRW1::EIPManager                             ! Set the EIP manager
  SELF.AddEditControl(EditInPlace::CJE:Sifra_usluge,1)
  SELF.AddEditControl(EditInPlace::CJE:Naziv_usluge,2)
  SELF.AddEditControl(EditInPlace::CJE:Cijena_usluge,3)
  SELF.DeleteAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisMjesta PROCEDURE 

BRW1::View:Browse    VIEW(MJESTO)
                       PROJECT(MJE:Postanski_broj)
                       PROJECT(MJE:Naziv_mjesta)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
MJE:Postanski_broj     LIKE(MJE:Postanski_broj)       !List box control field - type derived from field
MJE:Naziv_mjesta       LIKE(MJE:Naziv_mjesta)         !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis mjesta'),AT(0,0,247,140),CENTERED,ICON('DIZAJN\Pocetni prozor\preuzmi.ico'), |
  GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(5,5,235,100),USE(?List),HVSCROLL,FORMAT('74C|M~Postanski broj~@P## ###P@120C|M~' & |
  'Naziv mjesta~@s30@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(5,110,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(50,110,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(95,110,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(145,110,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(200,110,40,12),USE(?Close)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::EIPManager     BrowseEIPManager                      ! Browse EIP Manager for Browse using ?List
EditInPlace::MJE:Postanski_broj EditEntryClass             ! Edit-in-place class for field MJE:Postanski_broj
EditInPlace::MJE:Naziv_mjesta EditEntryClass               ! Edit-in-place class for field MJE:Naziv_mjesta

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
  GlobalErrors.SetProcedureName('PopisMjesta')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:MJESTO.Open                                       ! File MJESTO used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:MJESTO,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,MJE:PK_Mjesto_PostBr)                 ! Add the sort order for MJE:PK_Mjesto_PostBr for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,MJE:Postanski_broj,1,BRW1)     ! Initialize the browse locator using  using key: MJE:PK_Mjesto_PostBr , MJE:Postanski_broj
  BRW1.AddField(MJE:Postanski_broj,BRW1.Q.MJE:Postanski_broj) ! Field MJE:Postanski_broj is a hot field or requires assignment from browse
  BRW1.AddField(MJE:Naziv_mjesta,BRW1.Q.MJE:Naziv_mjesta)  ! Field MJE:Naziv_mjesta is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisMjesta',BrowseWindow)                 ! Restore window settings from non-volatile store
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:MJESTO.Close
  END
  IF SELF.Opened
    INIMgr.Update('PopisMjesta',BrowseWindow)              ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeMjesta
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  SELF.EIP &= BRW1::EIPManager                             ! Set the EIP manager
  SELF.AddEditControl(EditInPlace::MJE:Postanski_broj,1)
  SELF.AddEditControl(EditInPlace::MJE:Naziv_mjesta,2)
  SELF.DeleteAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
PopisKlijenata PROCEDURE 

BRW1::View:Browse    VIEW(KLIJENT)
                       PROJECT(KLI:Sifra_klijenta)
                       PROJECT(KLI:OIB_klijenta)
                       PROJECT(KLI:Ime_klijenta)
                       PROJECT(KLI:Prezime_klijenta)
                       PROJECT(KLI:Spol_klijenta)
                       PROJECT(KLI:Puta_popravljao)
                       PROJECT(KLI:Ukupno_potrosio_sivanje)
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
KLI:Sifra_klijenta     LIKE(KLI:Sifra_klijenta)       !List box control field - type derived from field
KLI:OIB_klijenta       LIKE(KLI:OIB_klijenta)         !List box control field - type derived from field
KLI:Ime_klijenta       LIKE(KLI:Ime_klijenta)         !List box control field - type derived from field
KLI:Prezime_klijenta   LIKE(KLI:Prezime_klijenta)     !List box control field - type derived from field
KLI:Spol_klijenta      LIKE(KLI:Spol_klijenta)        !List box control field - type derived from field
KLI:Puta_popravljao    LIKE(KLI:Puta_popravljao)      !List box control field - type derived from field
KLI:Ukupno_potrosio_sivanje LIKE(KLI:Ukupno_potrosio_sivanje) !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Popis klijenata'),AT(0,0,372,140),TILED,ICON('DIZAJN\Pocetni prozor\network.ico'), |
  GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\500_F_117640876_fKFU67xr4JpkhqIRTs41oUosS3aUWQh1.jpg'),IMM
                       LIST,AT(5,5,356,100),USE(?List),HVSCROLL,FORMAT('80C|M~Sifra klijenta~@n4@89C|M~OIB kli' & |
  'jenta~@P#{11}P@120C|M~Ime klijenta~@s30@120C|M~Prezime klijenta~@s30@80C|M~Spol klij' & |
  'enta~@s20@93C|M~Puta popravljao~@n4@28C|M~Ukupno potrosio sivanje~@N7.2@'),FROM(Queue:Browse), |
  IMM,MSG('Browsing Records')
                       BUTTON('Unos'),AT(5,113,40,12),USE(?Insert)
                       BUTTON('Izmjena'),AT(50,113,40,12),USE(?Change),DEFAULT
                       BUTTON('Brisanje'),AT(95,113,40,12),USE(?Delete)
                       BUTTON('Odabir'),AT(173,113,40,12),USE(?Select)
                       BUTTON('IZLAZ'),AT(313,113,40,12),USE(?Close)
                     END

BRW1::LastSortOrder       BYTE
BRW1::SortHeader  CLASS(SortHeaderClassType) !Declare SortHeader Class
QueueResorted          PROCEDURE(STRING pString),VIRTUAL
                  END
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
SetAlerts              PROCEDURE(),DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
SetSort                PROCEDURE(BYTE NewOrder,BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator

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
  GlobalErrors.SetProcedureName('PopisKlijenata')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:KLIJENT.Open                                      ! File KLIJENT used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:KLIJENT,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1.AddSortOrder(,KLI:PK_Klijent_SifraKlijenta)         ! Add the sort order for KLI:PK_Klijent_SifraKlijenta for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,KLI:Sifra_klijenta,1,BRW1)     ! Initialize the browse locator using  using key: KLI:PK_Klijent_SifraKlijenta , KLI:Sifra_klijenta
  BRW1.AddField(KLI:Sifra_klijenta,BRW1.Q.KLI:Sifra_klijenta) ! Field KLI:Sifra_klijenta is a hot field or requires assignment from browse
  BRW1.AddField(KLI:OIB_klijenta,BRW1.Q.KLI:OIB_klijenta)  ! Field KLI:OIB_klijenta is a hot field or requires assignment from browse
  BRW1.AddField(KLI:Ime_klijenta,BRW1.Q.KLI:Ime_klijenta)  ! Field KLI:Ime_klijenta is a hot field or requires assignment from browse
  BRW1.AddField(KLI:Prezime_klijenta,BRW1.Q.KLI:Prezime_klijenta) ! Field KLI:Prezime_klijenta is a hot field or requires assignment from browse
  BRW1.AddField(KLI:Spol_klijenta,BRW1.Q.KLI:Spol_klijenta) ! Field KLI:Spol_klijenta is a hot field or requires assignment from browse
  BRW1.AddField(KLI:Puta_popravljao,BRW1.Q.KLI:Puta_popravljao) ! Field KLI:Puta_popravljao is a hot field or requires assignment from browse
  BRW1.AddField(KLI:Ukupno_potrosio_sivanje,BRW1.Q.KLI:Ukupno_potrosio_sivanje) ! Field KLI:Ukupno_potrosio_sivanje is a hot field or requires assignment from browse
  INIMgr.Fetch('PopisKlijenata',BrowseWindow)              ! Restore window settings from non-volatile store
  BRW1.AskProcedure = 1                                    ! Will call: AzuriranjeKlijenata
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
  !Initialize the Sort Header using the Browse Queue and Browse Control
  BRW1::SortHeader.Init(Queue:Browse,?List,'','',BRW1::View:Browse)
  BRW1::SortHeader.UseSortColors = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:KLIJENT.Close
  !Kill the Sort Header
  BRW1::SortHeader.Kill()
  END
  IF SELF.Opened
    INIMgr.Update('PopisKlijenata',BrowseWindow)           ! Save window data to non-volatile store
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    AzuriranjeKlijenata
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.SetAlerts PROCEDURE

  CODE
  PARENT.SetAlerts
  !Initialize the Sort Header using the Browse Queue and Browse Control
  BRW1::SortHeader.SetAlerts()


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  !Take Sort Headers Events
  IF BRW1::SortHeader.TakeEvents()
     RETURN Level:Notify
  END
  ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  SELF.SelectControl = ?Select
  SELF.HideSelect = 1                                      ! Hide the select button when disabled
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW1.SetSort PROCEDURE(BYTE NewOrder,BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.SetSort(NewOrder,Force)
  IF BRW1::LastSortOrder<>NewOrder THEN
     BRW1::SortHeader.ClearSort()
  END
  BRW1::LastSortOrder=NewOrder
  RETURN ReturnValue

BRW1::SortHeader.QueueResorted       PROCEDURE(STRING pString)
  CODE
    IF pString = ''
       BRW1.RestoreSort()
       BRW1.ResetSort(True)
    ELSE
       BRW1.ReplaceSort(pString,BRW1::Sort0:Locator)
       BRW1.SetLocatorFromSort()
    END
!!! <summary>
!!! Generated from procedure template - Form
!!! </summary>
AzuriranjeMjesta PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::MJE:Record  LIKE(MJE:RECORD),THREAD
FormWindow           WINDOW('Azuriranje mjesta...'),AT(,,164,70),CENTERED,CENTER,ICON('DIZAJN\Pocetni prozor' & |
  '\preuzmi.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with-sewi' & |
  'ng-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Postanski broj:'),AT(5,8),USE(?MJE:Postanski_broj:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@P## ###P),AT(59,8,60,10),USE(MJE:Postanski_broj),REQ
                       PROMPT('Naziv mjesta:'),AT(5,30),USE(?MJE:Naziv_mjesta:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(54,30,60,10),USE(MJE:Naziv_mjesta),CAP,REQ
                       BUTTON('Spremi'),AT(4,52,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(49,52,40,12),USE(?Cancel)
                       STRING(@S40),AT(94,52,64),USE(ActionMessage),COLOR(00E0FFFFh)
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
  GlobalErrors.SetProcedureName('AzuriranjeMjesta')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MJE:Postanski_broj:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(MJE:Record,History::MJE:Record)
  SELF.AddHistoryField(?MJE:Postanski_broj,1)
  SELF.AddHistoryField(?MJE:Naziv_mjesta,2)
  SELF.AddUpdateFile(Access:MJESTO)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:MJESTO.Open                                       ! File MJESTO used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:MJESTO
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
  INIMgr.Fetch('AzuriranjeMjesta',FormWindow)              ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:MJESTO.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeMjesta',FormWindow)           ! Save window data to non-volatile store
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
AzuriranjeTkanina PROCEDURE 

ActionMessage        CSTRING(40)                           !
History::TKA:Record  LIKE(TKA:RECORD),THREAD
FormWindow           WINDOW('Azuriranje tkanina...'),AT(,,170,93),CENTERED,CENTER,ICON('DIZAJN\Pocetni prozo' & |
  'r\fabric.ico'),GRAY,MDI,SYSTEM,WALLPAPER('DIZAJN\46068146-seamless-pattern-with-sewi' & |
  'ng-supplies-in-doodle-style-vintage-background-with-images-of-sewing-machi.jpg'),IMM
                       PROMPT('Sifra tkanine:'),AT(6,8),USE(?TKA:Sifra_tkanine:Prompt),FONT(,,COLOR:Black,FONT:regular), |
  COLOR(00E0FFFFh)
                       ENTRY(@n4),AT(52,8,60,10),USE(TKA:Sifra_tkanine),RIGHT(1),REQ
                       PROMPT('Naziv tkanine:'),AT(6,32),USE(?TKA:Naziv_tkanine:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(58,32,60,10),USE(TKA:Naziv_tkanine),CAP,REQ
                       PROMPT('Boja tkanine:'),AT(6,54),USE(?TKA:Boja_tkanine:Prompt),COLOR(00E0FFFFh)
                       ENTRY(@s30),AT(52,54,60,10),USE(TKA:Boja_tkanine),CAP,REQ
                       BUTTON('Spremi'),AT(3,73,40,12),USE(?OK),DEFAULT,REQ
                       BUTTON('Odustani'),AT(48,73,40,12),USE(?Cancel)
                       STRING(@S40),AT(93,73,70),USE(ActionMessage),COLOR(00E0FFFFh)
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
  GlobalErrors.SetProcedureName('AzuriranjeTkanina')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?TKA:Sifra_tkanine:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(TKA:Record,History::TKA:Record)
  SELF.AddHistoryField(?TKA:Sifra_tkanine,1)
  SELF.AddHistoryField(?TKA:Naziv_tkanine,2)
  SELF.AddHistoryField(?TKA:Boja_tkanine,3)
  SELF.AddUpdateFile(Access:TKANINA)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:TKANINA.Open                                      ! File TKANINA used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:TKANINA
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
  INIMgr.Fetch('AzuriranjeTkanina',FormWindow)             ! Restore window settings from non-volatile store
  SELF.AddItem(ToolbarForm)
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:TKANINA.Close
  END
  IF SELF.Opened
    INIMgr.Update('AzuriranjeTkanina',FormWindow)          ! Save window data to non-volatile store
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

