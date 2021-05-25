

   MEMBER('Krojacnica.clw')                                ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE

                     MAP
                       INCLUDE('KROJACNICA003.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Report
!!! Slozeno izvjesce
!!! </summary>
IspisSvihPopravaka PROCEDURE 

Progress:Thermometer BYTE                                  !
LOC:brojac           LONG                                  !
Process:View         VIEW(KLIJENT)
                       PROJECT(KLI:Ime_klijenta)
                       PROJECT(KLI:OIB_klijenta)
                       PROJECT(KLI:Prezime_klijenta)
                       PROJECT(KLI:Sifra_klijenta)
                       JOIN(POP:PK_Popravlja_SifraKlijenta_SifraUsluge_Datum,KLI:Sifra_klijenta),INNER
                         PROJECT(POP:Broj_popravaka)
                         PROJECT(POP:Cijena_popravaka)
                         PROJECT(POP:Datum)
                         PROJECT(POP:Sifra_usluge)
                         PROJECT(POP:Sifra_krojaca)
                         JOIN(CJE:PK_Cjenik_SifraUsluge,POP:Sifra_usluge)
                           PROJECT(CJE:Naziv_usluge)
                         END
                         JOIN(KRO:PK_Krojac_SifraKrojaca,POP:Sifra_krojaca)
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
                       HEADER,AT(1000,1000,6250,1146),USE(?Header)
                         STRING('Popis svih popravaka po klijentima'),AT(948,208),USE(?STRING1),FONT(,20,,FONT:bold+FONT:italic)
                         STRING('Datum izvjestaja:'),AT(3604,635),USE(?ReportDatePrompt),TRN
                         STRING('<<-- Date Stamp -->'),AT(4781,635),USE(?ReportDateStamp),TRN
                         IMAGE('DIZAJN\Pocetni prozor\needle.png'),AT(62,208,687,625),USE(?IMAGE1)
                       END
breakKlijentSifra      BREAK(KLI:Sifra_klijenta),USE(?BREAK1)
breakPopravljaDatum      BREAK(POP:Datum),USE(?BREAK2)
                           HEADER,AT(0,0,6250,1896),USE(?GROUPHEADER1)
                             STRING('Klijent:'),AT(187,219),USE(?STRING2),FONT(,,,FONT:bold)
                             STRING('Sifra:'),AT(854,219),USE(?STRING3)
                             STRING('OIB:'),AT(187,479),USE(?STRING4)
                             STRING('Ime:'),AT(187,771),USE(?STRING5)
                             STRING('Prezime:'),AT(187,1031),USE(?STRING6)
                             STRING(@n4),AT(1240,219),USE(KLI:Sifra_klijenta),RIGHT(1)
                             STRING(@P###########P),AT(531,479),USE(KLI:OIB_klijenta),LEFT
                             STRING(@s30),AT(583,771,1698),USE(KLI:Ime_klijenta),LEFT
                             STRING(@s30),AT(854,1031,1521),USE(KLI:Prezime_klijenta),LEFT
                             STRING('Krojac:'),AT(3448,219),USE(?STRING7),FONT(,,,FONT:bold)
                             STRING(@n4),AT(4479,219),USE(KRO:Sifra_krojaca),RIGHT(1)
                             STRING('Sifra:'),AT(4094,219),USE(?STRING8)
                             STRING('Ime:'),AT(3448,479),USE(?STRING9)
                             STRING('Prezime:'),AT(3448,771),USE(?STRING10)
                             STRING(@s30),AT(3781,479,1719),USE(KRO:Ime_krojaca)
                             STRING(@s30),AT(4062,771,1406),USE(KRO:Prezime_krojaca)
                             STRING('Datum popravka'),AT(198,1427),USE(?STRING11)
                             STRING('Broj popravaka'),AT(5031,1427),USE(?STRING12)
                             STRING('Cijena popravaka'),AT(1740,1427),USE(?STRING13)
                             STRING('Naziv usluge'),AT(3448,1427),USE(?STRING18)
                             LINE,AT(52,1698,5958,0),USE(?LINE1)
                           END
Detail                     DETAIL,AT(0,0,6250,615),USE(?Detail)
                             STRING(@D6),AT(344,135),USE(POP:Datum)
                             STRING(@n4),AT(5260,135),USE(POP:Broj_popravaka),RIGHT(1)
                             STRING(@n7.2),AT(1833,135),USE(POP:Cijena_popravaka),DECIMAL(12)
                             STRING(@s30),AT(3781,135,1167),USE(CJE:Naziv_usluge)
                           END
                           FOOTER,AT(0,0,6250,573),USE(?GROUPFOOTER1)
                             STRING('Ukupan broj popravaka:'),AT(3604,167),USE(?STRING15),FONT(,,,FONT:bold)
                             STRING(@n4),AT(5260,167,760),USE(POP:Broj_popravaka,,?POP:Broj_popravaka:2),CNT,RESET(breakPopravljaDatum)
                             LINE,AT(198,94,5927,0),USE(?LINE2)
                           END
                         END
                         FOOTER,AT(0,0,6250,823),USE(?GROUPFOOTER2),PAGEAFTER(1)
                           STRING('Broj popravaka ovog klijenta:'),AT(156,146),USE(?STRING16),FONT(,,COLOR:White)
                           STRING('Ukupan broj svih popravaka:'),AT(156,500),USE(?STRING17),FONT(,,COLOR:White)
                           STRING(@n4),AT(1937,500),USE(POP:Broj_popravaka,,?POP:Broj_popravaka:3),FONT(,,COLOR:White), |
  CENTER
                           STRING(@n4),AT(1937,146),USE(LOC:brojac),FONT(,,COLOR:White),CENTER(1),CNT,TALLY(breakPopravljaDatum)
                         END
                       END
                       FOOTER,AT(1000,9688,6250,594),USE(?Footer)
                         STRING(@N3),AT(5417,177),USE(ReportPageNumber)
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
  GlobalErrors.SetProcedureName('IspisSvihPopravaka')
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
  INIMgr.Fetch('IspisSvihPopravaka',ProgressWindow)        ! Restore window settings from non-volatile store
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
    INIMgr.Update('IspisSvihPopravaka',ProgressWindow)     ! Save window data to non-volatile store
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

