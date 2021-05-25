   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('KROJACNICA_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('KROJACNICA001.CLW')
GlavniIzbornik         PROCEDURE   !Pocetni prozor aplikacije
     END
   END

SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
TKANINA              FILE,DRIVER('TOPSPEED'),PRE(TKA),CREATE,BINDABLE,THREAD !                    
SK_Tkanina_NazivTkanine  KEY(TKA:Naziv_tkanine),DUP,NOCASE !                    
SK_Tkanina_BojaTkanine   KEY(TKA:Boja_tkanine),DUP,NOCASE  !                    
PK_Tkanina_SifraTkanine  KEY(TKA:Sifra_tkanine),NOCASE,PRIMARY !                    
Record                   RECORD,PRE()
Sifra_tkanine               SHORT                          !                    
Naziv_tkanine               CSTRING(31)                    !                    
Boja_tkanine                CSTRING(31)                    !                    
                         END
                     END                       

SIVA                 FILE,DRIVER('TOPSPEED'),PRE(SIV),CREATE,BINDABLE,THREAD !                    
VK_Siva_Tkanina_SifraTkanine KEY(SIV:Sifra_tkanine),DUP,NOCASE !                    
PK_Siva_SifraKlijenta_SifraKrojaca_Datum KEY(SIV:Sifra_klijenta,SIV:Sifra_krojaca,SIV:Datum),NOCASE,PRIMARY !                    
RK_Siva_Datum_SifraKrojaca_SifraKlijenta KEY(SIV:Datum,SIV:Sifra_krojaca,SIV:Sifra_klijenta),NOCASE !                    
Record                   RECORD,PRE()
Sifra_tkanine               SHORT                          !                    
Sifra_klijenta              SHORT                          !                    
Sifra_krojaca               SHORT                          !                    
Datum                       DATE                           !                    
Cijena_sivanja              DECIMAL(7,2)                   !                    
Popust                      DECIMAL(7,2)                   !                    
Iznos_popusta               SHORT                          !                    
Iznos_sivanja_ukupno        DECIMAL(7,2)                   !                    
Vrijeme                     TIME                           !                    
Naziv_proizvoda             CSTRING(31)                    !                    
Velicina                    CSTRING(21)                    !                    
Napomena                    STRING(101)                    !                    
                         END
                     END                       

KLIJENT              FILE,DRIVER('TOPSPEED'),PRE(KLI),CREATE,BINDABLE,THREAD !                    
PK_Klijent_SifraKlijenta KEY(KLI:Sifra_klijenta),NOCASE,PRIMARY !                    
Record                   RECORD,PRE()
Sifra_klijenta              SHORT                          !                    
OIB_klijenta                STRING(20)                     !                    
Ime_klijenta                CSTRING(31)                    !                    
Prezime_klijenta            CSTRING(31)                    !                    
Spol_klijenta               STRING(20)                     !                    
Puta_popravljao             SHORT                          !                    
Ukupno_potrosio_sivanje     DECIMAL(7,2)                   !                    
                         END
                     END                       

POPRAVLJA            FILE,DRIVER('TOPSPEED'),PRE(POP),CREATE,BINDABLE,THREAD !                    
PK_Popravlja_SifraKlijenta_SifraUsluge_Datum KEY(POP:Sifra_klijenta,POP:Sifra_usluge,POP:Datum),NOCASE,PRIMARY !                    
VK_Popravlja_Krojac_SifraKrojaca KEY(POP:Sifra_krojaca),DUP,NOCASE !                    
Record                   RECORD,PRE()
Sifra_klijenta              SHORT                          !                    
Datum                       DATE                           !                    
Sifra_usluge                SHORT                          !                    
Broj_popravaka              SHORT                          !                    
Cijena_popravaka            DECIMAL(7,2)                   !                    
Vrijeme                     TIME                           !                    
Sifra_krojaca               SHORT                          !                    
                         END
                     END                       

CJENIK_USLUGA        FILE,DRIVER('TOPSPEED'),PRE(CJE),CREATE,BINDABLE,THREAD !                    
PK_Cjenik_SifraUsluge    KEY(CJE:Sifra_usluge),NOCASE,PRIMARY !                    
Record                   RECORD,PRE()
Sifra_usluge                SHORT                          !                    
Naziv_usluge                CSTRING(31)                    !                    
Cijena_usluge               DECIMAL(7,2)                   !                    
                         END
                     END                       

MJESTO               FILE,DRIVER('TOPSPEED'),PRE(MJE),CREATE,BINDABLE,THREAD !                    
PK_Mjesto_PostBr         KEY(MJE:Postanski_broj),NOCASE,PRIMARY !                    
Record                   RECORD,PRE()
Postanski_broj              STRING(20)                     !                    
Naziv_mjesta                CSTRING(31)                    !                    
                         END
                     END                       

KROJACNICA           FILE,DRIVER('TOPSPEED'),PRE(KROJ),CREATE,BINDABLE,THREAD !                    
SK_Krojacnica_NazivKrojacnice KEY(KROJ:Naziv_krojacnice),DUP,NOCASE !                    
PK_Krojacnica_SifraKrojacnice KEY(KROJ:Sifra_krojacnice),NOCASE,PRIMARY !                    
VK_Krojacnica_Mjesto_PostBr KEY(KROJ:Postanski_broj),DUP,NOCASE !                    
Record                   RECORD,PRE()
Sifra_krojacnice            SHORT                          !                    
Naziv_krojacnice            CSTRING(31)                    !                    
Adresa                      CSTRING(31)                    !                    
Postanski_broj              STRING(20)                     !                    
                         END
                     END                       

KROJAC               FILE,DRIVER('TOPSPEED'),PRE(KRO),CREATE,BINDABLE,THREAD !                    
PK_Krojac_SifraKrojaca   KEY(KRO:Sifra_krojaca),NOCASE,PRIMARY !                    
VK_Krojac_Krojacnica_SifraKrojacnice KEY(KRO:Sifra_krojacnice),DUP,NOCASE !                    
Record                   RECORD,PRE()
Sifra_krojaca               SHORT                          !                    
Ime_krojaca                 CSTRING(31)                    !                    
Prezime_krojaca             CSTRING(31)                    !                    
Spol_krojaca                CSTRING(21)                    !                    
Datum_rodjenja              DATE                           !                    
Strucna_sprema              CSTRING(31)                    !                    
Iznos_place                 DECIMAL(7,2)                   !                    
Sifra_krojacnice            SHORT                          !                    
                         END
                     END                       

!endregion

Access:TKANINA       &FileManager,THREAD                   ! FileManager for TKANINA
Relate:TKANINA       &RelationManager,THREAD               ! RelationManager for TKANINA
Access:SIVA          &FileManager,THREAD                   ! FileManager for SIVA
Relate:SIVA          &RelationManager,THREAD               ! RelationManager for SIVA
Access:KLIJENT       &FileManager,THREAD                   ! FileManager for KLIJENT
Relate:KLIJENT       &RelationManager,THREAD               ! RelationManager for KLIJENT
Access:POPRAVLJA     &FileManager,THREAD                   ! FileManager for POPRAVLJA
Relate:POPRAVLJA     &RelationManager,THREAD               ! RelationManager for POPRAVLJA
Access:CJENIK_USLUGA &FileManager,THREAD                   ! FileManager for CJENIK_USLUGA
Relate:CJENIK_USLUGA &RelationManager,THREAD               ! RelationManager for CJENIK_USLUGA
Access:MJESTO        &FileManager,THREAD                   ! FileManager for MJESTO
Relate:MJESTO        &RelationManager,THREAD               ! RelationManager for MJESTO
Access:KROJACNICA    &FileManager,THREAD                   ! FileManager for KROJACNICA
Relate:KROJACNICA    &RelationManager,THREAD               ! RelationManager for KROJACNICA
Access:KROJAC        &FileManager,THREAD                   ! FileManager for KROJAC
Relate:KROJAC        &RelationManager,THREAD               ! RelationManager for KROJAC

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\Krojacnica.INI', NVD_INI)                 ! Configure INIManager to use INI file
  DctInit
  GlavniIzbornik
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

