/* Interface for CO2 prediction program.  */

#pragma hdrfile "e:\bc\predict\predict.sym"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include <dos.h>
#include <conio.h>

#define _MATH
#define _STDIO
#define _STDLIB
#define _CTYPE

#define Uses_TEventQueue
#define Uses_TKeys
#define Uses_TApplication
#define Uses_TEvent
#define Uses_TRect
#define Uses_TDialog
#define Uses_TStaticText
#define Uses_TButton
#define Uses_TMenuBar
#define Uses_TSubMenu
#define Uses_TMenuItem
#define Uses_TMenu
#define Uses_TMenuBox
#define Uses_TStatusLine
#define Uses_TStatusItem
#define Uses_TStatusDef
#define Uses_TDeskTop
#define Uses_TProgram
#define Uses_TView
#define Uses_TWindow
#define Uses_TInputLine
#define Uses_TLabel
#define Uses_TRadioButtons
#define Uses_TCluster
#define Uses_TSItem
#define Uses_TFileDialog
#define Uses_TFileWindow
#include <tv.h>

#pragma hdrstop
#include <john.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <sys\stat.h>

#define NUMLINE 11
typedef char diastring[NUMLINE];

const AboutCmd		= 100;
const PatternCmd	= 101;
const ReservoirCmd= 102;
const FluidsCmd	= 103;
const KrgCmd		= 104;
const KrogCmd		= 105;
const KrwCmd		= 106;
const KrowCmd		= 107;
const KrsCmd		= 108;
const OoipCmd		= 109;
const CalcOoipCmd	= 110;
const PAmountsCmd	= 111;
const CBoundCmd	= 112;
const CLocateCmd	= 113;
const CAmountsCmd	= 114;
const InjectionCmd= 115;
const SoiCmd		= 116;
const FreqCmd		= 117;
const PlotCmd		= 118;
const SatCmd		= 119;	//Do it all in saturations menu
const DoitCmd		= 120;
const KrmCmd		= 121;
const OmegaCmd		= 122;
const LayerCmd		= 123;
const Krs1Cmd		= 124;
const FileCmd		= 125;
const ProdCmd		= 126;
const SaveFileCmd	= 127;
const DoPatternCmd= 128;
const TitleCmd		= 129;

const hcFluid1		= 1000;
const hcFluid2		= 1001;

struct checkData{			// structure to handle checkboxes
	ushort box;
	};

checkData *PatternData;
checkData *ReportData;
checkData *KrmData;
checkData *StreamData;

struct SixData{
	diastring data[6];
	};

SixData* FluidData1;

struct Title{
	char title[55];
	};

Title* TitleData;

struct FData{					// structure to handle Fluid Data menus
	diastring data[5];
	};

FData* FluidData2;			// Fluid Data pointer

struct FourData{				// Structure for four data values
	diastring data[4];
	};

FourData* KrwData;			// Krw initialization data
FourData* Krs1Data;			// Krs initialization data

struct ThreeData{				// Structure for three data values
	diastring data[3];
	};

ThreeData* KrowData;			// Krow initialization data
ThreeData* KrgData;			// Krg initialization data
ThreeData* CalcOoipData;

struct TwoData{				// Structure for two data values
	diastring data[2];
	};

TwoData* KrogData;			// Krog initialization data

struct OneData{  				// Stupid structure for one data value
	diastring data;
	};

OneData* KrsData;				// Krs initialization data
OneData* OoipData;			// Ooip Initialization data
OneData* OmegaData;			// Omega initialization data
OneData* LayerData;			// Layer initialization data
OneData* SoiData;				// Present oil saturation

struct Quad{					// Structure for Amounts and rates dialog
	diastring data[4][4];
	ushort box;
	};

Quad* PAmtData;
Quad* Inj1RateData;

struct EightData{
	diastring data[8];
	};

EightData* Inj2RateData;

struct TenData{
	diastring data[10];
	};

TenData* ProdRateData;

struct TwentyData{
	diastring data[20];
	};

TwentyData* BoundData;
TwentyData* InjWellData;
TwentyData* ProdWellData;


void beep(void);

double datastor[40];			// Space to hold all the numeric data
int intstor[6];
int ooipflag=0;
int firstflag=0;					//Should the streamtubes be generated
double boundstor[11][2];		//XY of custom pattern vertecies
double injwellstor[11][2];		//XY location of injection wells
double prodwellstor[11][2];	//XY location of production wells
double injwellrate[10][4][2];	//Water and co2 rates
double prodwellrate[10];		//Total allocated fluid produced
double injamount[4][2];			//WAG ratio and HCPV injected
double injwellq[10];				//Injection Qs for streamtubes
double prodwellq[10];			//Production Qs for streamtubes


// Stuff for the error file viewer
const int maxLineLength = 81;
const int maxLines      = 15;
char *lines[maxLines];
int lineCount = 0;


void readFile( const char *fileName )
{
    ifstream fileToView( fileName );
    if( !fileToView )
        {
        cout << "Invalid file name..." << endl;
        exit( 1 );
        }
    else
        {
        char buf[maxLineLength];
        while( lineCount < maxLines &&
               fileToView.getline( buf, maxLineLength ) != 0 )
            {
            lines[lineCount] = newStr( buf );
            lineCount++;
            }
        }
}

void deleteFile()
{
    for( int i = 0; i < lineCount; i++ )
        delete lines[i];
}


//******************************
// Define the object for the application
//******************************

class TPredApp : public TApplication
{
	char pat[5];
	char lwgen[4];
	char wagtag[4];
	double ooip;


public:
	TPredApp();
	~TPredApp();
	virtual void handleEvent(TEvent& event);
	static TMenuBar *initMenuBar(TRect);
	static TStatusLine *initStatusLine(TRect);
	virtual void idle();
	void Mistake();
	void PatternDialog();
	void FluidDialog1();
	void FluidDialog2();
	void KrwDialog();
	void KrowDialog();
	void KrgDialog();
	void KrogDialog();
	void KrsDialog();
	void WriteFile(char *fp);
	int  ReadFile();
	int  SaveFile();
	void PAmtDialog();
	void OoipDialog();
	void Krs1Dialog();
	void ReportDialog();
	void OmegaDialog();
	void LayerDialog();
	void KrmDialog();
	void SoiDialog();
	void BoundDialog();
	void InjWellDialog();
	void ProdWellDialog();
	void InjRateDialog();
	void CalcOoipDialog();
	void ProdWellRates();
	void ViewError();
	void TitleDialog();
	void StreamDialog();
};

//*******************************
// Constructor for TPredApp
//*******************************

TPredApp::TPredApp() :
	TProgInit( &TPredApp::initStatusLine,
					&TPredApp::initMenuBar,
					&TPredApp::initDeskTop
				)
{
	int i;
// Pattern defaults
	strcpy(pat,"'5S'");	//Default 5 spot
	strcpy(lwgen,"'N'");	//Default to not generate streamtubes
	strcpy(wagtag,"'T'");//Default to time specification for WAG
	PatternData=new checkData;
	PatternData->box=0;
	intstor[0]=2;
	intstor[1]=1;
	intstor[2]=5;
// Make the space and initialize the Fluid Data defaults
	FluidData1=new SixData;
	strcpy(FluidData1->data[5],"0.7");
	datastor[5]=0.7;						//DPCOEF
	strcpy(FluidData1->data[0],"100");
	datastor[0]=100;						//TRES
	strcpy(FluidData1->data[1],"2000");
	datastor[1]=2000;						//P
	strcpy(FluidData1->data[2],"1200");
	datastor[2]=1200;						//MMP
	strcpy(FluidData1->data[3],"2.0");
	datastor[3]=2.0;						//VISO
	strcpy(FluidData1->data[4],"1.4");
	datastor[4]=1.4;						//BO
	FluidData2=new FData;
	strcpy(FluidData2->data[4],"500");
	datastor[10]=500;						//RS
	strcpy(FluidData2->data[0],"26");
	datastor[6]=26;						//API
	strcpy(FluidData2->data[1],"0.7");
	datastor[7]=0.7;						//GSG
	strcpy(FluidData2->data[2],"0.8");
	datastor[8]=0.8;						//VISW
	strcpy(FluidData2->data[3],"100000");
	datastor[9]=100000;					//SALN
// Make space and initialize the Krw defaults
	KrwData=new FourData;
	strcpy(KrwData->data[0],"0.20");
	datastor[11]=0.2;						//SWIR
	strcpy(KrwData->data[1],"0.37");
	datastor[12]=0.37;					//SORW
	strcpy(KrwData->data[2],"2.0");
	datastor[13]=2.0;						//EXPW
	strcpy(KrwData->data[3],"0.30");
	datastor[14]=0.3;						//KWRO
// Make space and initialize the Krow defaults
	KrowData=new ThreeData;
	strcpy(KrowData->data[0],"0.20");
	datastor[15]=0.2;						//SWC
	strcpy(KrowData->data[1],"2.0");
	datastor[16]=2.0;						//EXPOW
	strcpy(KrowData->data[2],"0.40");
	datastor[17]=0.4;						//KROCW
// Make space and initialize the Krg defaults
	KrgData=new ThreeData;
	strcpy(KrgData->data[0],"0.37");
	datastor[18]=0.37;					//SGR
	strcpy(KrgData->data[1],"2.0");
	datastor[19]=2.0;						//EXPG
	strcpy(KrgData->data[2],"0.40");
	datastor[20]=0.4;						//KRGCW
// Make space and initialize the Krog defaults
	KrogData=new TwoData;
	strcpy(KrogData->data[0],"2.0");
	datastor[21]=2.0;						//EXPOG
	strcpy(KrogData->data[1],"0.37");
	datastor[22]=0.37;					//SORG
// Make space and initialize the Krs defaults
	KrsData=new OneData;
	strcpy(KrsData->data,"0.001");
	datastor[24]=0.001;					//SORM
// Make space and initialize the Amounts and Rates
	PAmtData=new Quad;
	for (i=0;i<16;i++)
		PAmtData->data[0][i][0]='\0';
	PAmtData->box=0;
// Make spacd and initialize the OOIP dialog
	OoipData=new OneData;
	strcpy(OoipData->data,"0.0");
	ooip=0.0;
// Make space and initialize the Krs1 dialog
	Krs1Data=new FourData;
	strcpy(Krs1Data->data[0],"0.37");
	datastor[23]=0.37;						//SSR
	strcpy(Krs1Data->data[1],"0.001");
	datastor[24]=0.001; 					//SORM
	strcpy(Krs1Data->data[2],"2.0");
	datastor[25]=2.0;						//EXPS
	strcpy(Krs1Data->data[3],"0.4");
	datastor[26]=0.4;						//KRSMAX
// Make space and initialize the Report dialog
	ReportData=new checkData;
	ReportData->box=0;
	datastor[27]=1.0;						//OUTTIM
// Make space and initializ the Omega dialog
	OmegaData=new OneData;
	strcpy(OmegaData->data,"0.666");
	datastor[28]=0.666;					//W
// Make space and initialize the Layer dialog
	LayerData=new OneData;
	strcpy(LayerData->data,"5");
	intstor[4]=5;							//NLAYERS
// Make space and initialize the Krm dialog
	KrmData=new checkData;
	KrmData->box=2;						//KRMSEL
	intstor[3]=2;
// Make space and initialize the Soi dialog
	SoiData=new OneData;
	strcpy(SoiData->data,"0.75");
	datastor[29]=0.75;					//SOINIT
	datastor[30]=0.0;						//SGINIT
	datastor[31]=0.25;					//SWINIT
// Make space for Boundary dialog
	BoundData=new TwentyData;
	for (i=0;i<20;i++){
		BoundData->data[i][0]='\0';
		}
// Make space for Injection well location dialog
	InjWellData=new TwentyData;
	for (i=0;i<20;i++){
		InjWellData->data[i][0]='\0';
		}
// Make space for Injection well location dialog
	ProdWellData=new TwentyData;
	for (i=0;i<20;i++){
		ProdWellData->data[i][0]='\0';
		}
// Make space for injection rate dialog
	Inj1RateData=new Quad;
	for (i=0;i<16;i++)
		Inj1RateData->data[0][i][0]='\0';
	Inj1RateData->box=0;
// Space for future expansion
	datastor[32]=10.0;					//PERMAV
	datastor[33]=0.1;						//XKVH
// Make space and initialize calcooipdialog
	CalcOoipData=new ThreeData;
	strcpy(CalcOoipData->data[2],"10");
	datastor[34]=435600;						//AREA
	strcpy(CalcOoipData->data[0],"10");
	datastor[35]=10;						//THICKNESS
	strcpy(CalcOoipData->data[1],"0.2");
	datastor[36]=0.2;						//POROSITY
// Make space and initialize ProdWellRates dialog
	ProdRateData=new TenData;
	for (i=0;i<10;i++)
		ProdRateData->data[i][0]='\0';
// Make sure that the title string is NULL
	TitleData=new Title;
	for (i=0;i<20;i++)
		TitleData->title[i]='\0';
// Make space and initialize the rest of the custom injection dialog
	Inj2RateData=new EightData;
	for (i=0;i<8;i++)
		Inj2RateData->data[i][0]='\0';
// Make space and initialize stream dialog
	StreamData=new checkData;
	StreamData->box=0;
};

//*******************************
// Destructor for string data
//*******************************

TPredApp::~TPredApp()
{
	delete(FluidData1);
	delete(FluidData2);
	delete(PatternData);
	delete(KrwData);
	delete(KrowData);
	delete(KrgData);
	delete(KrogData);
	delete(KrsData);
	delete(PAmtData);
	delete(OoipData);
	delete(Krs1Data);
	delete(ReportData);
	delete(OmegaData);
	delete(LayerData);
	delete(KrmData);
	delete(SoiData);
};



//***************************
// RadioButtons to Select which Pattern
//***************************
void TPredApp::PatternDialog()
{
	TDialog *p=new TDialog(TRect(24,5,56,17),"Patterns");
	p->helpCtx=1000;
	if (p){

		p->insert(new TButton(TRect(18,9,28,11),"~C~ancel",cmCancel,bfNormal));
		p->insert(new TButton(TRect(4,9,14,11),"~O~K",cmOK,bfDefault));

		TView *a=new TRadioButtons(TRect(8,2,24,8),
			new TSItem("5 Spot",
			new TSItem("7 Spot",
			new TSItem("9 Spot",
			new TSItem("Line Drive",
			new TSItem("4 Spot",
			new TSItem("2 Spot",0)))))));
		p->insert(a);

		p->setData(PatternData);
		ushort control=deskTop->execView(p);
		if (control!=cmCancel)
			p->getData(PatternData);
		}
	destroy(p);
	switch (PatternData->box)
	{
		case 0:
			strcpy(pat,"'5S'");
			intstor[0]=2;
			break;
		case 1:
			strcpy(pat,"'7S'");
			intstor[0]=3;
			break;
		case 2:
			strcpy(pat,"'9S'");
			intstor[0]=4;
			break;
		case 3:
			strcpy(pat,"'LD'");
			intstor[0]=2;
			break;
		case 4:
			strcpy(pat,"'4S'");
			intstor[0]=2;
			break;
		case 5:
			strcpy(pat,"'2S'");
			intstor[0]=1;
			break;
		default:
			strcpy(pat,"'5S'");
			intstor[0]=2;
		}
	intstor[1]=1;
	intstor[2]=5;
	strcpy(lwgen,"'N'");
}

//***************************
// Fluids Dialog 1
//***************************
void TPredApp::FluidDialog1()
{
	TDialog *fd1=new TDialog(TRect(5,2,63,19),"Reservoir Data");

	if (fd1){

		fd1->helpCtx=hcFluid1;

		TView *a=new TInputLine(TRect(37,4,47,5),NUMLINE);
		fd1->insert(a);
		fd1->insert(new TLabel(TRect(3,4,35,5),"Reservoir Temperature",a));
		fd1->insert(new TLabel(TRect(48,4,56,5),"øF",a));

		TView *b=new TInputLine(TRect(37,6,47,7),NUMLINE);
		fd1->insert(b);
		fd1->insert(new TLabel(TRect(3,6,35,7),"Average Reservoir Pressure",b));
		fd1->insert(new TLabel(TRect(48,6,56,7),"psia",b));

		TView *c=new TInputLine(TRect(37,8,47,9),NUMLINE);
		fd1->insert(c);
		fd1->insert(new TLabel(TRect(3,8,35,9),"Minimum Misciblity Pressure",c));
		fd1->insert(new TLabel(TRect(48,8,56,9),"psia",c));

		TView *d=new TInputLine(TRect(37,10,47,11),NUMLINE);
		fd1->insert(d);
		fd1->insert(new TLabel(TRect(3,10,35,11),"Oil Viscosity",d));
		fd1->insert(new TLabel(TRect(48,10,56,11),"cP",d));

		TView *e=new TInputLine(TRect(37,12,47,13),NUMLINE);
		fd1->insert(e);
		fd1->insert(new TLabel(TRect(3,12,35,13),"Oil Formation Volume"
			                                       " Factor, Bo",e));
		fd1->insert(new TLabel(TRect(48,12,56,13),"RB/STB",e));

		fd1->insert(new TButton(TRect(12,14,22,16),"~O~k",cmOK,bfDefault));
		fd1->insert(new TButton(TRect(33,14,43,16),"~C~ancel",cmCancel,bfNormal));

		TView *f=new TInputLine(TRect(37,2,47,3),NUMLINE);
		fd1->insert(f);
		fd1->insert(new TLabel(TRect(3,2,35,3),"Dykstra-Parsons Coeficient",f));
		fd1->insert(new TLabel(TRect(48,2,56,3),"",f));

		fd1->setData(FluidData1);

		ushort control=deskTop->execView(fd1);

		if (control!=cmCancel)
			fd1->getData(FluidData1);
		}

	destroy(fd1);
	for (int i=0,error=0;i<6;i++){
		if (verify(FluidData1->data[i],&datastor[i])){
			error=1;
			}
		}
	if (error){
		Mistake();
		FluidDialog1();
		}
}


//***************************
// Fluids Dialog 2
//***************************
void TPredApp::FluidDialog2()
{
	TDialog *fd2=new TDialog(TRect(13,5,71,20),"More Reservoir Data");

	if (fd2){

		fd2->helpCtx=hcFluid2;

		TView *b=new TInputLine(TRect(37,4,47,5),NUMLINE);
		fd2->insert(b);
		fd2->insert(new TLabel(TRect(3,4,35,5),"Oil Gravity, API",b));
		fd2->insert(new TLabel(TRect(48,4,56,5),"øAPI",b));

		TView *c=new TInputLine(TRect(37,6,47,7),NUMLINE);
		fd2->insert(c);
		fd2->insert(new TLabel(TRect(3,6,35,7),"Gas Specific Gravity",c));
		fd2->insert(new TLabel(TRect(48,6,56,7),"Air=1",c));

		TView *d=new TInputLine(TRect(37,8,47,9),NUMLINE);
		fd2->insert(d);
		fd2->insert(new TLabel(TRect(3,8,35,9),"Water Viscosity",d));
		fd2->insert(new TLabel(TRect(48,8,56,9),"cP",d));

		TView *e=new TInputLine(TRect(37,10,47,11),NUMLINE);
		fd2->insert(e);
		fd2->insert(new TLabel(TRect(3,10,35,11),"Water Salinity",e));
		fd2->insert(new TLabel(TRect(48,10,56,11),"ppm",e));

		fd2->insert(new TButton(TRect(12,12,22,14),"~O~k",cmOK,bfDefault));
		fd2->insert(new TButton(TRect(33,12,43,14),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(37,2,47,3),NUMLINE);
		fd2->insert(a);
		fd2->insert(new TLabel(TRect(3,2,35,3),"Solution Gas-Oil Ratio, Rs",a));
		fd2->insert(new TLabel(TRect(48,2,56,3),"scf/STB",a));

		fd2->setData(FluidData2);

		ushort control=deskTop->execView(fd2);

		if (control!=cmCancel)
			fd2->getData(FluidData2);
		}

	destroy(fd2);
	for (int i=0,error=0,j=6;i<5;i++,j++){
		if (verify(FluidData2->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		FluidDialog2();
		}
}

//***************************
// Krw Dialog
//***************************
void TPredApp::KrwDialog()
{
	TDialog *krw=new TDialog(TRect(9,1,71,22),"Water Relative Permeability");
	krw->helpCtx=1000;

	if (krw){
		krw->insert(new TStaticText(TRect(22,3,47,4),"Ú                       ¿"));
		krw->insert(new TStaticText(TRect(22,4,47,5),"³    Sw -               ³"));
		krw->insert(new TStaticText(TRect(3,5,47,6),"Krw =            * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krw->insert(new TStaticText(TRect(22,6,47,7),"³ 1 -            - Swir ³"));
		krw->insert(new TStaticText(TRect(22,7,47,8),"À                       Ù"));

		TView *c=new TInputLine(TRect(32,4,42,5),NUMLINE);
		krw->insert(c);
//		krw->insert(new TLabel(TRect(33,3,38,4),"Swir",c));
		krw->insert(new TLabel(TRect(3,11,53,12),"Swir = "
			                        "Irreducible water saturation",c));

		TView *d=new TInputLine(TRect(28,6,38,7),NUMLINE);
		krw->insert(d);
//		krw->insert(new TLabel(TRect(31,7,36,8),"Sorw",d));
		krw->insert(new TLabel(TRect(3,13,53,14),"Sorw = Residual oil"
											" saturation to water",d));

		TView *b=new TInputLine(TRect(48,3,58,4),NUMLINE);
		krw->insert(b);
//		krw->insert(new TLabel(TRect(50,2,55,3),"Expw",b));
		krw->insert(new TLabel(TRect(3,15,53,16),"Expw = Water exponent",b));

		krw->insert(new TButton(TRect(15,18,25,20),"~O~k",cmOK,bfDefault));
		krw->insert(new TButton(TRect(35,18,45,20),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(9,5,19,6),NUMLINE);
		krw->insert(a);
//		krw->insert(new TLabel(TRect(11,4,16,5),"Kwro",a));
		krw->insert(new TLabel(TRect(3,9,59,10),"Kwro = Rel perm to water"
											" at residual oil saturation",a));

		krw->setData(KrwData);

		ushort control=deskTop->execView(krw);

		if (control!=cmCancel)
			krw->getData(KrwData);
		}
	destroy(krw);
	for (int i=0,error=0,j=11;i<4;i++,j++){
		if (verify(KrwData->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		KrwDialog();
		}
}


//***************************
// Krow Dialog
//***************************
void TPredApp::KrowDialog()
{
	TDialog *krow=new TDialog(TRect(9,2,71,21),"Oil-Water Relative Permeability");
	krow->helpCtx=1000;

	if (krow){
		krow->insert(new TStaticText(TRect(22,3,47,4),"Ú                       ¿"));
		krow->insert(new TStaticText(TRect(22,4,47,5),"³     1 - Sw - Sorw     ³"));
		krow->insert(new TStaticText(TRect(2,5,47,6),"Krow =            * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krow->insert(new TStaticText(TRect(22,6,47,7),"³ 1 - Sorw -            ³"));
		krow->insert(new TStaticText(TRect(22,7,47,8),"À                       Ù"));


		TView *d=new TInputLine(TRect(35,6,45,7),NUMLINE);
		krow->insert(d);
//		krow->insert(new TLabel(TRect(31,7,36,8),"Swc",d));
		krow->insert(new TLabel(TRect(3,11,53,12),"Swc = Connate water saturation",d));

		TView *b=new TInputLine(TRect(48,3,58,4),NUMLINE);
		krow->insert(b);
//		krow->insert(new TLabel(TRect(50,2,55,3),"Expow",b));
		krow->insert(new TLabel(TRect(3,13,53,14),"Expow = Oil exponent",b));

		krow->insert(new TButton(TRect(15,16,25,18),"~O~k",cmOK,bfDefault));
		krow->insert(new TButton(TRect(35,16,45,18),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(9,5,19,6),NUMLINE);
		krow->insert(a);
//		krow->insert(new TLabel(TRect(11,4,16,5),"Krocw",a));
		krow->insert(new TLabel(TRect(3,9,59,10),"Krocw = Rel perm to oil"
											" at connate water saturation",a));

		krow->setData(KrowData);

		ushort control=deskTop->execView(krow);

		if (control!=cmCancel)
			krow->getData(KrowData);
		}
	destroy(krow);
	for (int i=0,error=0,j=15;i<3;i++,j++){
		if (verify(KrowData->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		KrowDialog();
		}
}



//***************************
// Krg Dialog
//***************************
void TPredApp::KrgDialog()
{
	TDialog *krg=new TDialog(TRect(9,2,71,21),"Gas Relative Permeability");
	krg->helpCtx=1000;

	if (krg){
		krg->insert(new TStaticText(TRect(22,3,47,4),"Ú                 ¿"));
		krg->insert(new TStaticText(TRect(22,4,47,5),"³ Sg -            ³"));
		krg->insert(new TStaticText(TRect(3,5,47,6),"Krg =            * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krg->insert(new TStaticText(TRect(22,6,47,7),"³  1 - Swc - Sgr  ³"));
		krg->insert(new TStaticText(TRect(22,7,47,8),"À                 Ù"));


		TView *d=new TInputLine(TRect(29,4,39,5),NUMLINE);
		krg->insert(d);
//		krg->insert(new TLabel(TRect(31,7,36,8),"Sgr",d));
		krg->insert(new TLabel(TRect(3,11,53,12),"Sgr = Residual gas saturation",d));

		TView *b=new TInputLine(TRect(42,3,52,4),NUMLINE);
		krg->insert(b);
//		krg->insert(new TLabel(TRect(50,2,55,3),"Expg",b));
		krg->insert(new TLabel(TRect(3,13,53,14),"Expg = Gas exponent",b));

		krg->insert(new TButton(TRect(15,16,25,18),"~O~k",cmOK,bfDefault));
		krg->insert(new TButton(TRect(35,16,45,18),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(9,5,19,6),NUMLINE);
		krg->insert(a);
//		krg->insert(new TLabel(TRect(11,4,16,5),"Krgcw",a));
		krg->insert(new TLabel(TRect(3,9,59,10),"Krgcw = Rel perm to gas"
											" at connate water saturation",a));

		krg->setData(KrgData);

		ushort control=deskTop->execView(krg);

		if (control!=cmCancel)
			krg->getData(KrgData);
		}
	destroy(krg);
	for (int i=0,error=0,j=18;i<3;i++,j++){
		if (verify(KrgData->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		KrgDialog();
		}
}


//***************************
// Krog Dialog
//***************************
void TPredApp::KrogDialog()
{
	TDialog *krog=new TDialog(TRect(9,2,71,19),"Oil-Gas Relative Permeability");
	krog->helpCtx=1000;

	if (krog){
		krog->insert(new TStaticText(TRect(18,3,48,4),"Ú                           ¿"));
		krog->insert(new TStaticText(TRect(18,4,48,5),"³ 1 - Swc -            - Sg ³"));
		krog->insert(new TStaticText(TRect(3,5,48,6),"Krog = Krocw * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krog->insert(new TStaticText(TRect(18,6,48,7),"³      1 - Swc - Sorg       ³"));
		krog->insert(new TStaticText(TRect(18,7,48,8),"À                           Ù"));


		TView *b=new TInputLine(TRect(48,3,58,4),NUMLINE);
		krog->insert(b);
//		krog->insert(new TLabel(TRect(50,2,55,3),"Expog",b));
		krog->insert(new TLabel(TRect(3,11,53,12),"Expog = Oil-Gas exponent",b));

		krog->insert(new TButton(TRect(15,14,25,16),"~O~k",cmOK,bfDefault));
		krog->insert(new TButton(TRect(35,14,45,16),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(30,4,40,5),NUMLINE);
		krog->insert(d);
//		krog->insert(new TLabel(TRect(35,7,45,8),"Sorg",d));
		krog->insert(new TLabel(TRect(3,9,53,10),"Sorg = Residual oil "
														"saturation to gas",d));

		krog->setData(KrogData);

		ushort control=deskTop->execView(krog);

		if (control!=cmCancel)
			krog->getData(KrogData);
		}
	destroy(krog);
	for (int i=0,error=0,j=21;i<2;i++,j++){
		if (verify(KrogData->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		KrogDialog();
		}
}



//***************************
// Krs Dialog
//***************************
void TPredApp::KrsDialog()
{
	TDialog *krs=new TDialog(TRect(9,2,71,17),"Solvent Relative Permeability");
	krs->helpCtx=1000;

	if (krs){
		krs->insert(new TStaticText(TRect(17,3,55,4),"Ú                              ¿ Exps"));
		krs->insert(new TStaticText(TRect(17,4,50,5),"³          Sg - Ssr            ³"));
		krs->insert(new TStaticText(TRect(3,5,50,6),"Krs = Ksmax * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krs->insert(new TStaticText(TRect(17,6,50,7),"³ 1 - Swir - Ssr -             ³"));
		krs->insert(new TStaticText(TRect(17,7,50,8),"À                              Ù"));


		krs->insert(new TButton(TRect(15,12,25,14),"~O~k",cmOK,bfDefault));
		krs->insert(new TButton(TRect(35,12,45,14),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(36,6,46,7),NUMLINE);
		krs->insert(d);
//		krs->insert(new TLabel(TRect(35,6,45,7),"Sorm",d));
		krs->insert(new TLabel(TRect(3,9,53,10),"Sorm = Residual oil "
														"saturation to solvent",d));

		krs->setData(KrsData);

		ushort control=deskTop->execView(krs);

		if (control!=cmCancel)
			krs->getData(KrsData);
		}
	destroy(krs);
	int error=0;
	if (verify(KrsData->data,&datastor[24]))
		error=1;
	datastor[23]=datastor[12];		// Set Ssr=Sorw
	datastor[25]=datastor[19];		// Set Exps=Expg
	datastor[26]=datastor[20];		// Set Krsmax=Krgcw
	if (error){
		Mistake();
		KrsDialog();
		}
}



//*******************************
// Preset Pattern Amounts Dialog
//*******************************
void TPredApp::PAmtDialog(){
	int error;
	double wag;

	TDialog *amt=new TDialog(TRect(5,1,75,22),"Pre-Set Pattern Rates"
									" and Volumes");

	for (int i=0;i<4;i++){
		for (int j=0;j<2;j++){
			if	(injwellrate[0][i][j]>0)
				sprintf(PAmtData->data[i][j],"%.2lf",injwellrate[0][i][j]);
			else
				PAmtData->data[i][j][0]='\0';
			}
		}

//Convert TMORVL to WAG and to characters
	for (i=0;i<4;i++){
		if (injamount[i][0]>0)		//HCPV
			sprintf(PAmtData->data[i][3],"%.2lf",injamount[i][0]);
		else
			PAmtData->data[i][3][0]='\0';

		if (injamount[i][0]>0){
			wag=1-injamount[i][1];
			if (wag<1e-8 || fabs(wag-1)<1e-8)
				PAmtData->data[i][2][0]='\0';
			else
				sprintf(PAmtData->data[i][2],"%.2lf",1/wag-1);
			}

		else
			PAmtData->data[i][2][0]='\0';

		}

	amt->helpCtx=1000;
	if (amt){
		amt->insert(new TStaticText(TRect(5,2,68,3),"         Injection Rates"
			"                     Amount Injected"));
		amt->insert(new TStaticText(TRect(5,3,68,4),"      ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
			"ÍÍÍ    Water/CO2   Water (Waterflood)"));
		amt->insert(new TStaticText(TRect(5,4,68,5),"        Water        CO2  "
			"      Injection   or CO2 (CO2 or WAG)"));
		amt->insert(new TStaticText(TRect(2,5,68,6),"Period    bbls/day   MMSCF/"
			"day       Ratio            HCPV"));
		amt->insert(new TStaticText(TRect(2,6,68,7),"ÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄ"
			"ÄÄÄÄ    ÄÄÄÄÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"));
		amt->insert(new TStaticText(TRect(3,7,6,8)," 1"));
		amt->insert(new TStaticText(TRect(3,9,6,10)," 2"));
		amt->insert(new TStaticText(TRect(3,11,6,12)," 3"));
		amt->insert(new TStaticText(TRect(3,13,6,14)," 4"));
		amt->insert(new TStaticText(TRect(47,16,65,17),"Injection Ratio"));

		TView *b=new TInputLine(TRect(11,7,21,8),NUMLINE);
		amt->insert(b);

		TView *f=new TInputLine(TRect(23,7,33,8),NUMLINE);
		amt->insert(f);

		TView *j=new TInputLine(TRect(37,7,47,8),NUMLINE);
		amt->insert(j);

		TView *n=new TInputLine(TRect(53,7,63,8),NUMLINE);
		amt->insert(n);

		TView *c=new TInputLine(TRect(11,9,21,10),NUMLINE);
		amt->insert(c);

		TView *g=new TInputLine(TRect(23,9,33,10),NUMLINE);
		amt->insert(g);

		TView *k=new TInputLine(TRect(37,9,47,10),NUMLINE);
		amt->insert(k);

		TView *o=new TInputLine(TRect(53,9,63,10),NUMLINE);
		amt->insert(o);

		TView *d=new TInputLine(TRect(11,11,21,12),NUMLINE);
		amt->insert(d);

		TView *h=new TInputLine(TRect(23,11,33,12),NUMLINE);
		amt->insert(h);

		TView *l=new TInputLine(TRect(37,11,47,12),NUMLINE);
		amt->insert(l);

		TView *p=new TInputLine(TRect(53,11,63,12),NUMLINE);
		amt->insert(p);

		TView *e=new TInputLine(TRect(11,13,21,14),NUMLINE);
		amt->insert(e);

		TView *i=new TInputLine(TRect(23,13,33,14),NUMLINE);
		amt->insert(i);

		TView *m=new TInputLine(TRect(37,13,47,14),NUMLINE);
		amt->insert(m);

		TView *q=new TInputLine(TRect(53,13,63,14),NUMLINE);
		amt->insert(q);


		amt->insert(new TButton(TRect(10,17,20,19),"~O~k",cmOK,bfDefault));
		amt->insert(new TButton(TRect(25,17,35,19),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TRadioButtons(TRect(45,17,65,19),
			new TSItem("Time Basis",
			new TSItem("Volume Basis",0)));
		amt->insert(a);

		amt->setData(PAmtData);

		ushort control=deskTop->execView(amt);

		if (control!=cmCancel){
			amt->getData(PAmtData);
			}
		}
	destroy(amt);
	double trash;

	for (i=0,error=0;i<4;i++){
		for (int j=0;j<2;j++){
			if (verify(PAmtData->data[i][j],&injwellrate[0][i][j]))
				error=1;
			}
		}
	for (i=0,intstor[5]=0;i<4;i++){
		if (verify(PAmtData->data[i][3],&injamount[i][0]))	//HCPV
			error=1;
		if (verify(PAmtData->data[i][2],&injamount[i][1]))	//WAG
			error=1;
		if (injamount[i][0]>0)
			intstor[5]++;
		}
//Convert WAG to TMORVL

	for (i=0;i<4;i++){
		injamount[i][1]=1-1/(injamount[i][1]+1);
		if (injwellrate[0][i][0]>0){
			if (!injwellrate[0][i][1]>0)
				injamount[i][1]=1.0;
			}
		else{
			injamount[i][1]=0.0;
			}
		}

	if (PAmtData->box==0)
		strcpy(wagtag,"'T'");
	else
		strcpy(wagtag,"'V'");

	if (error){
		Mistake();
		PAmtDialog();
		}
	}

//*******************************
// OOIP Dialog
//*******************************
void TPredApp::OoipDialog()
{
	TDialog *oip=new TDialog(TRect(17,6,63,16),"Oil In Place");

	oip->helpCtx=1000;
	if (oip){
		oip->insert(new TStaticText(TRect(6,4,43,5),"Area, Thickness, and Porosity will"));
		oip->insert(new TStaticText(TRect(6,5,43,6)," be assigned appropriate values."));

		oip->insert(new TButton(TRect(9,7,19,9),"~O~k",cmOK,bfDefault));
		oip->insert(new TButton(TRect(25,7,35,9),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(26,2,38,3),NUMLINE);
		oip->insert(d);
		oip->insert(new TLabel(TRect(38,2,43,3),"STB",d));
		oip->insert(new TLabel(TRect(1,2,25,3),"Original Oil In Place =",d));

		oip->setData(OoipData);

		ushort control=deskTop->execView(oip);

		if (control!=cmCancel){
			oip->getData(OoipData);
			ooipflag=1;
			}
		}
	destroy(oip);
	int error=0;
	if (verify(OoipData->data,&ooip))
		error=1;
	if (error){
		Mistake();
		OoipDialog();
		}
}


//***************************
// Krs1 Dialog
//***************************
void TPredApp::Krs1Dialog()
{
	TDialog *krs1=new TDialog(TRect(6,1,74,22),"Solvent-Oil Relative Permeability");
	krs1->helpCtx=1000;

	if (krs1){
		krs1->insert(new TStaticText(TRect(22,3,55,4),"Ú                             ¿"));
		krs1->insert(new TStaticText(TRect(22,4,55,5),"³      Sg -                   ³"));
		krs1->insert(new TStaticText(TRect(3,5,55,6),"Krs =            * ³ "
			"ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³"));
		krs1->insert(new TStaticText(TRect(22,6,55,7),"³ 1 - Swir - Ssr -            ³"));
		krs1->insert(new TStaticText(TRect(22,7,55,8),"À                             Ù"));

		TView *c=new TInputLine(TRect(34,4,44,5),NUMLINE);
		krs1->insert(c);
//		krs1->insert(new TLabel(TRect(33,3,38,4),"Ssr",c));
		krs1->insert(new TLabel(TRect(3,11,53,12),"Ssr = "
			                        "Irreducible solvent saturation",c));

		TView *d=new TInputLine(TRect(41,6,51,7),NUMLINE);
		krs1->insert(d);
//		krs1->insert(new TLabel(TRect(40,7,50,8),"Sorm",d));
		krs1->insert(new TLabel(TRect(3,13,53,14),"Sorm = Residual oil"
											" saturation to solvent",d));

		TView *b=new TInputLine(TRect(54,3,64,4),NUMLINE);
		krs1->insert(b);
//		krs1->insert(new TLabel(TRect(43,2,55,3),"Exps",b));
		krs1->insert(new TLabel(TRect(3,15,53,16),"Exps = Solvent exponent",b));

		krs1->insert(new TButton(TRect(17,18,27,20),"~O~k",cmOK,bfDefault));
		krs1->insert(new TButton(TRect(38,18,48,20),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(9,5,19,6),NUMLINE);
		krs1->insert(a);
//		krs1->insert(new TLabel(TRect(11,4,16,5),"Ksmax",a));
		krs1->insert(new TLabel(TRect(3,9,59,10),"Ksmax = Maximum solvent"
											" relative permeability",a));

		krs1->setData(Krs1Data);

		ushort control=deskTop->execView(krs1);

		if (control!=cmCancel)
			krs1->getData(Krs1Data);
		}
	destroy(krs1);
	for (int i=0,error=0,j=23;i<4;i++,j++){
		if (verify(Krs1Data->data[i],&datastor[j])){
			error=1;
			}
		}
	if (error){
		Mistake();
		Krs1Dialog();
		}
}


//***************************
// RadioButtons to Select Report Frequency
//***************************
void TPredApp::ReportDialog()
{
	TDialog *rep=new TDialog(TRect(24,5,56,15),"Report Frequency");
	rep->helpCtx=1000;
	if (rep){

		rep->insert(new TButton(TRect(18,7,28,9),"~C~ancel",cmCancel,bfNormal));
		rep->insert(new TButton(TRect(4,7,14,9),"~O~K",cmOK,bfDefault));

		TView *a=new TRadioButtons(TRect(6,2,26,6),
			new TSItem("Annually",
			new TSItem("Semi-Annually",
			new TSItem("Quarterly",
			new TSItem("Monthly",0)))));
		rep->insert(a);

		rep->setData(ReportData);
		ushort control=deskTop->execView(rep);
		if (control!=cmCancel)
			rep->getData(ReportData);
		}
	destroy(rep);
	switch (ReportData->box)
	{
		case 0:
			datastor[27]=1.0;
			break;
		case 1:
			datastor[27]=0.5;
			break;
		case 2:
			datastor[27]=0.25;
			break;
		case 3:
			datastor[27]=0.0833;
			break;
		}
}


//*******************************
// Omega Dialog
//*******************************
void TPredApp::OmegaDialog()
{
	TDialog *om=new TDialog(TRect(17,6,63,14),"Mixing Parameter");

	om->helpCtx=1000;
	if (om){
		om->insert(new TButton(TRect(9,5,19,7),"~O~k",cmOK,bfDefault));
		om->insert(new TButton(TRect(25,5,35,7),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(31,2,41,3),NUMLINE);
		om->insert(d);
		om->insert(new TLabel(TRect(3,2,29,3),"Mixing Parameter, Omega =",d));

		om->setData(OmegaData);

		ushort control=deskTop->execView(om);

		if (control!=cmCancel){
			om->getData(OmegaData);
			}
		}
	destroy(om);
	int error=0;
	if (verify(OmegaData->data,&datastor[28]))
		error=1;
	else
		if (datastor[28]<0 || datastor[28]>1.0)
		error=1;
	if (error){
		Mistake();
		OmegaDialog();
		}
}

//*******************************
// Layers Dialog
//*******************************
void TPredApp::LayerDialog()
{
	TDialog *lay=new TDialog(TRect(21,6,59,14),"Layers");
	lay->helpCtx=1000;

	if (lay){
		lay->insert(new TButton(TRect(5,5,15,7),"~O~k",cmOK,bfDefault));
		lay->insert(new TButton(TRect(21,5,31,7),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(24,2,34,3),NUMLINE);
		lay->insert(d);
		lay->insert(new TLabel(TRect(3,2,22,3),"Number of Layers =",d));

		lay->setData(LayerData);

		ushort control=deskTop->execView(lay);

		if (control!=cmCancel){
			lay->getData(LayerData);
			}
		}
	destroy(lay);
	int error=0;
	double trash;
	if (verify(LayerData->data,&trash))
		error=1;
	else
		intstor[4]=trash;
	if (error){
		Mistake();
		LayerDialog();
		}
}


//***************************
// RadioButtons to Select Krm calculation
//***************************
void TPredApp::KrmDialog()
{
	TDialog *krm=new TDialog(TRect(15,5,65,14),"Miscible Rel Perm, Krm");
	krm->helpCtx=1000;
	if (krm){

		krm->insert(new TButton(TRect(28,6,38,8),"~C~ancel",cmCancel,bfNormal));
		krm->insert(new TButton(TRect(12,6,22,8),"~O~K",cmOK,bfDefault));

		TView *a=new TRadioButtons(TRect(6,2,44,5),
			new TSItem("Saturation weighted Krow and Krs",
			new TSItem("Average of Krow and Krg",
			new TSItem("Equal to Krow",0))));
		krm->insert(a);

		krm->setData(KrmData);
		ushort control=deskTop->execView(krm);
		if (control!=cmCancel)
			krm->getData(KrmData);
			intstor[3]=KrmData->box;
			}
	destroy(krm);
}

//*******************************
// Soi Dialog
//*******************************
void TPredApp::SoiDialog()
{
	TDialog *krm=new TDialog(TRect(17,6,63,14),"Present Oil Saturation");
	krm->helpCtx=1000;

	if (krm){
		krm->insert(new TButton(TRect(9,5,19,7),"~O~k",cmOK,bfDefault));
		krm->insert(new TButton(TRect(25,5,35,7),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(33,2,43,3),NUMLINE);
		krm->insert(d);
		krm->insert(new TLabel(TRect(2,2,33,3),"Present Oil Saturation, Soi =",d));

		krm->setData(SoiData);

		ushort control=deskTop->execView(krm);

		if (control!=cmCancel){
			krm->getData(SoiData);
			}
		}
	destroy(krm);
	int error=0;
	if (verify(SoiData->data,&datastor[29]))
		error=1;
	else
		if (datastor[29]<0 || datastor[29]>1.0)
		error=1;
	datastor[31]=1-datastor[29];
	if (error){
		Mistake();
		SoiDialog();
		}
}


//*******************************
// Bounds Dialog
//*******************************
void TPredApp::BoundDialog()
{
	diastring temp;
	TDialog *bnd=new TDialog(TRect(4,2,76,18),"Pattern Vertices");

	bnd->helpCtx=1000;
	if (bnd){

		bnd->insert(new TStaticText(TRect(9,2,65,3),"X             Y"
			         "                        X             Y"));

		TView *a1=new TInputLine(TRect(19,3,29,4),NUMLINE);
		bnd->insert(a1);

		TView *b=new TInputLine(TRect(5,5,15,6),NUMLINE);
		bnd->insert(b);
		bnd->insert(new TLabel(TRect(1,5,4,6),"2",b));

		TView *b1=new TInputLine(TRect(19,5,29,6),NUMLINE);
		bnd->insert(b1);

		TView *c=new TInputLine(TRect(5,7,15,8),NUMLINE);
		bnd->insert(c);
		bnd->insert(new TLabel(TRect(1,7,4,8),"3",c));

		TView *c1=new TInputLine(TRect(19,7,29,8),NUMLINE);
		bnd->insert(c1);

		TView *d=new TInputLine(TRect(5,9,15,10),NUMLINE);
		bnd->insert(d);
		bnd->insert(new TLabel(TRect(1,9,4,10),"4",d));

		TView *d1=new TInputLine(TRect(19,9,29,10),NUMLINE);
		bnd->insert(d1);

		TView *e=new TInputLine(TRect(5,11,15,12),NUMLINE);
		bnd->insert(e);
		bnd->insert(new TLabel(TRect(1,11,4,12),"5",e));

		TView *e1=new TInputLine(TRect(19,11,29,12),NUMLINE);
		bnd->insert(e1);

		TView *f=new TInputLine(TRect(44,3,54,4),NUMLINE);
		bnd->insert(f);
		bnd->insert(new TLabel(TRect(40,3,43,4),"6",f));

		TView *f1=new TInputLine(TRect(58,3,68,4),NUMLINE);
		bnd->insert(f1);

		TView *g=new TInputLine(TRect(44,5,54,6),NUMLINE);
		bnd->insert(g);
		bnd->insert(new TLabel(TRect(40,5,43,6),"7",g));

		TView *g1=new TInputLine(TRect(58,5,68,6),NUMLINE);
		bnd->insert(g1);

		TView *h=new TInputLine(TRect(44,7,54,8),NUMLINE);
		bnd->insert(h);
		bnd->insert(new TLabel(TRect(40,7,43,8),"8",h));

		TView *h1=new TInputLine(TRect(58,7,68,8),NUMLINE);
		bnd->insert(h1);

		TView *i=new TInputLine(TRect(44,9,54,10),NUMLINE);
		bnd->insert(i);
		bnd->insert(new TLabel(TRect(40,9,43,10),"9",i));

		TView *i1=new TInputLine(TRect(58,9,68,10),NUMLINE);
		bnd->insert(i1);

		TView *j=new TInputLine(TRect(44,11,54,12),NUMLINE);
		bnd->insert(j);
		bnd->insert(new TLabel(TRect(40,11,43,12),"10",j));

		TView *j1=new TInputLine(TRect(58,11,68,12),NUMLINE);
		bnd->insert(j1);

		bnd->insert(new TButton(TRect(20,13,30,15),"~O~k",cmOK,bfDefault));
		bnd->insert(new TButton(TRect(40,13,50,15),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(5,3,15,4),NUMLINE);
		bnd->insert(a);
		bnd->insert(new TLabel(TRect(1,3,4,4),"1",a));

		bnd->setData(BoundData);

		ushort control=deskTop->execView(bnd);

		if (control!=cmCancel){
			bnd->getData(BoundData);
			strcpy(pat,"'CS'");
			strcpy(lwgen,"'Y'");
			}
		}
	destroy(bnd);

	int count=0;
	int error=0;
	int rev;

	strcpy(temp,BoundData->data[19]);
	for (rev=19;rev>0;rev--){
		strcpy(BoundData->data[rev],BoundData->data[rev-1]);
		}
	strcpy(BoundData->data[0],temp);

	for (int ii=0, jj=0;ii<20;ii++,ii++,jj++){
		if (strlen(BoundData->data[ii])){
			if (verify(BoundData->data[ii],&boundstor[jj][0]))
				error=1;
			else{
				if (strlen(BoundData->data[ii+1])){
					if (verify(BoundData->data[ii+1],&boundstor[jj][1]))
						error=1;
					else
						count++;
					}
				else
					error=1;
				}
			}
		}
	strcpy(temp,BoundData->data[0]);
	for (rev=0;rev<19;rev++){
		strcpy(BoundData->data[rev],BoundData->data[rev+1]);
		}
	strcpy(BoundData->data[19],temp);
	boundstor[count][0]=boundstor[0][0];	//Close the polygon
	boundstor[count][1]=boundstor[0][1];
	intstor[2]=count+1;

	if (error){
		Mistake();
		BoundDialog();
		}
}

//*******************************
// Injection Well Location Dialog
//*******************************
void TPredApp::InjWellDialog()
{
	diastring temp;
	TDialog *iwl=new TDialog(TRect(4,2,76,18),"Injection Well Locations");

	iwl->helpCtx=1000;
	if (iwl){

		iwl->insert(new TStaticText(TRect(9,2,65,3),"X             Y"
			         "                        X             Y"));

		TView *a1=new TInputLine(TRect(19,3,29,4),NUMLINE);
		iwl->insert(a1);

		TView *b=new TInputLine(TRect(5,5,15,6),NUMLINE);
		iwl->insert(b);
		iwl->insert(new TLabel(TRect(1,5,4,6),"2",b));

		TView *b1=new TInputLine(TRect(19,5,29,6),NUMLINE);
		iwl->insert(b1);

		TView *c=new TInputLine(TRect(5,7,15,8),NUMLINE);
		iwl->insert(c);
		iwl->insert(new TLabel(TRect(1,7,4,8),"3",c));

		TView *c1=new TInputLine(TRect(19,7,29,8),NUMLINE);
		iwl->insert(c1);

		TView *d=new TInputLine(TRect(5,9,15,10),NUMLINE);
		iwl->insert(d);
		iwl->insert(new TLabel(TRect(1,9,4,10),"4",d));

		TView *d1=new TInputLine(TRect(19,9,29,10),NUMLINE);
		iwl->insert(d1);

		TView *e=new TInputLine(TRect(5,11,15,12),NUMLINE);
		iwl->insert(e);
		iwl->insert(new TLabel(TRect(1,11,4,12),"5",e));

		TView *e1=new TInputLine(TRect(19,11,29,12),NUMLINE);
		iwl->insert(e1);

		TView *f=new TInputLine(TRect(44,3,54,4),NUMLINE);
		iwl->insert(f);
		iwl->insert(new TLabel(TRect(40,3,43,4),"6",f));

		TView *f1=new TInputLine(TRect(58,3,68,4),NUMLINE);
		iwl->insert(f1);

		TView *g=new TInputLine(TRect(44,5,54,6),NUMLINE);
		iwl->insert(g);
		iwl->insert(new TLabel(TRect(40,5,43,6),"7",g));

		TView *g1=new TInputLine(TRect(58,5,68,6),NUMLINE);
		iwl->insert(g1);

		TView *h=new TInputLine(TRect(44,7,54,8),NUMLINE);
		iwl->insert(h);
		iwl->insert(new TLabel(TRect(40,7,43,8),"8",h));

		TView *h1=new TInputLine(TRect(58,7,68,8),NUMLINE);
		iwl->insert(h1);

		TView *i=new TInputLine(TRect(44,9,54,10),NUMLINE);
		iwl->insert(i);
		iwl->insert(new TLabel(TRect(40,9,43,10),"9",i));

		TView *i1=new TInputLine(TRect(58,9,68,10),NUMLINE);
		iwl->insert(i1);

		TView *j=new TInputLine(TRect(44,11,54,12),NUMLINE);
		iwl->insert(j);
		iwl->insert(new TLabel(TRect(40,11,43,12),"10",j));

		TView *j1=new TInputLine(TRect(58,11,68,12),NUMLINE);
		iwl->insert(j1);

		iwl->insert(new TButton(TRect(20,13,30,15),"~O~k",cmOK,bfDefault));
		iwl->insert(new TButton(TRect(40,13,50,15),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(5,3,15,4),NUMLINE);
		iwl->insert(a);
		iwl->insert(new TLabel(TRect(1,3,4,4),"1",a));

		iwl->setData(InjWellData);

		ushort control=deskTop->execView(iwl);

		if (control!=cmCancel){
			iwl->getData(InjWellData);
			strcpy(lwgen,"'Y'");
			}
		}
	destroy(iwl);

	int count=0;
	int error=0;
	int rev;

	strcpy(temp,InjWellData->data[19]);
	for (rev=19;rev>0;rev--){
		strcpy(InjWellData->data[rev],InjWellData->data[rev-1]);
		}
	strcpy(InjWellData->data[0],temp);

	for (int ii=0, jj=0;ii<20;ii++,ii++,jj++){
		if (strlen(InjWellData->data[ii])){
			if (verify(InjWellData->data[ii],&injwellstor[jj][0]))
				error=1;
			else{
				if (strlen(InjWellData->data[ii+1])){
					if (verify(InjWellData->data[ii+1],&injwellstor[jj][1]))
						error=1;
					else
						count++;
					}
				else
					error=1;
				}
			}
		}
	strcpy(temp,InjWellData->data[0]);
	for (rev=0;rev<19;rev++){
		strcpy(InjWellData->data[rev],InjWellData->data[rev+1]);
		}
	strcpy(InjWellData->data[19],temp);

	intstor[1]=count;

	if (error){
		Mistake();
		InjWellDialog();
		}
}


//*******************************
// Production Well Location Dialog
//*******************************
void TPredApp::ProdWellDialog()
{
	diastring temp;
	TDialog *pwl=new TDialog(TRect(4,2,76,18),"Production Well Locations");

	pwl->helpCtx=1000;
	if (pwl){

		pwl->insert(new TStaticText(TRect(9,2,65,3),"X             Y"
			         "                        X             Y"));

		TView *a1=new TInputLine(TRect(19,3,29,4),NUMLINE);
		pwl->insert(a1);

		TView *b=new TInputLine(TRect(5,5,15,6),NUMLINE);
		pwl->insert(b);
		pwl->insert(new TLabel(TRect(1,5,4,6),"2",b));

		TView *b1=new TInputLine(TRect(19,5,29,6),NUMLINE);
		pwl->insert(b1);

		TView *c=new TInputLine(TRect(5,7,15,8),NUMLINE);
		pwl->insert(c);
		pwl->insert(new TLabel(TRect(1,7,4,8),"3",c));

		TView *c1=new TInputLine(TRect(19,7,29,8),NUMLINE);
		pwl->insert(c1);

		TView *d=new TInputLine(TRect(5,9,15,10),NUMLINE);
		pwl->insert(d);
		pwl->insert(new TLabel(TRect(1,9,4,10),"4",d));

		TView *d1=new TInputLine(TRect(19,9,29,10),NUMLINE);
		pwl->insert(d1);

		TView *e=new TInputLine(TRect(5,11,15,12),NUMLINE);
		pwl->insert(e);
		pwl->insert(new TLabel(TRect(1,11,4,12),"5",e));

		TView *e1=new TInputLine(TRect(19,11,29,12),NUMLINE);
		pwl->insert(e1);

		TView *f=new TInputLine(TRect(44,3,54,4),NUMLINE);
		pwl->insert(f);
		pwl->insert(new TLabel(TRect(40,3,43,4),"6",f));

		TView *f1=new TInputLine(TRect(58,3,68,4),NUMLINE);
		pwl->insert(f1);

		TView *g=new TInputLine(TRect(44,5,54,6),NUMLINE);
		pwl->insert(g);
		pwl->insert(new TLabel(TRect(40,5,43,6),"7",g));

		TView *g1=new TInputLine(TRect(58,5,68,6),NUMLINE);
		pwl->insert(g1);

		TView *h=new TInputLine(TRect(44,7,54,8),NUMLINE);
		pwl->insert(h);
		pwl->insert(new TLabel(TRect(40,7,43,8),"8",h));

		TView *h1=new TInputLine(TRect(58,7,68,8),NUMLINE);
		pwl->insert(h1);

		TView *i=new TInputLine(TRect(44,9,54,10),NUMLINE);
		pwl->insert(i);
		pwl->insert(new TLabel(TRect(40,9,43,10),"9",i));

		TView *i1=new TInputLine(TRect(58,9,68,10),NUMLINE);
		pwl->insert(i1);

		TView *j=new TInputLine(TRect(44,11,54,12),NUMLINE);
		pwl->insert(j);
		pwl->insert(new TLabel(TRect(40,11,43,12),"10",j));

		TView *j1=new TInputLine(TRect(58,11,68,12),NUMLINE);
		pwl->insert(j1);

		pwl->insert(new TButton(TRect(20,13,30,15),"~O~k",cmOK,bfDefault));
		pwl->insert(new TButton(TRect(40,13,50,15),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(5,3,15,4),NUMLINE);
		pwl->insert(a);
		pwl->insert(new TLabel(TRect(1,3,4,4),"1",a));

		pwl->setData(ProdWellData);

		ushort control=deskTop->execView(pwl);

		if (control!=cmCancel){
			pwl->getData(ProdWellData);
			strcpy(lwgen,"'Y'");
			}
		}
	destroy(pwl);

	int count=0;
	int error=0;
	int rev;

	strcpy(temp,ProdWellData->data[19]);
	for (rev=19;rev>0;rev--){
		strcpy(ProdWellData->data[rev],ProdWellData->data[rev-1]);
		}
	strcpy(ProdWellData->data[0],temp);

	for (int ii=0, jj=0;ii<20;ii++,ii++,jj++){
		if (strlen(ProdWellData->data[ii])){
			if (verify(ProdWellData->data[ii],&prodwellstor[jj][0]))
				error=1;
			else{
				if (strlen(ProdWellData->data[ii+1])){
					if (verify(ProdWellData->data[ii+1],&prodwellstor[jj][1]))
						error=1;
					else
						count++;
					}
				else
					error=1;
				}
			}
		}
	strcpy(temp,ProdWellData->data[0]);
	for (rev=0;rev<19;rev++){
		strcpy(ProdWellData->data[rev],ProdWellData->data[rev+1]);
		}
	strcpy(ProdWellData->data[19],temp);
	intstor[0]=count+intstor[1];
	if (error){
		Mistake();
		ProdWellDialog();
		}
}

//*******************************
// Injection Rates Dialog
//*******************************
void TPredApp::InjRateDialog(){
	int error;
	double wag;

	char display[50];
	sprintf(display,"Injection Well #1     X=%-10.2lf  Y=%-10.2lf",
		injwellstor[0][0],injwellstor[0][1]);

	for (int i=0;i<4;i++){
		for (int j=0;j<2;j++){
			if	(injwellrate[0][i][j]>0)
				sprintf(Inj1RateData->data[i][j],"%.2lf",injwellrate[0][i][j]);
			else
				Inj1RateData->data[i][j][0]='\0';
			}
		}
	for (i=0;i<4;i++){
		if (injamount[i][0]>0)		//HCPV
			sprintf(Inj1RateData->data[i][3],"%.2lf",injamount[i][0]);
		else
			Inj1RateData->data[i][3][0]='\0';

		if (injamount[i][0]>0){
			wag=1-injamount[i][1];
			if (wag<1e-8 || fabs(wag-1)<1e-8)
				Inj1RateData->data[i][2][0]='\0';
			else
				sprintf(Inj1RateData->data[i][2],"%.2lf",1/wag-1);
			}

		else
			Inj1RateData->data[i][2][0]='\0';

		}


	TDialog *amt=new TDialog(TRect(5,1,75,22),"Custom Pattern Allocated Rates");
	amt->helpCtx=1000;
	if (amt){
		amt->insert(new TStaticText(TRect(1,1,68,2),display));
		amt->insert(new TStaticText(TRect(5,3,68,4),"         Allocated Rates"
			"                     Amount Injected"));
		amt->insert(new TStaticText(TRect(5,4,68,5),"      ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
			"ÍÍÍ    Water/CO2   Water (Waterflood)"));
		amt->insert(new TStaticText(TRect(5,5,68,6),"        Water        CO2  "
			"      Injection   or CO2 (CO2 or WAG)"));
		amt->insert(new TStaticText(TRect(2,6,68,7),"Period    bbls/day   MMSCF/"
			"day       Ratio            HCPV"));
		amt->insert(new TStaticText(TRect(2,7,68,8),"ÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄ"
			"ÄÄÄÄ    ÄÄÄÄÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"));
		amt->insert(new TStaticText(TRect(3,8,6,9)," 1"));
		amt->insert(new TStaticText(TRect(3,10,6,11)," 2"));
		amt->insert(new TStaticText(TRect(3,12,6,13)," 3"));
		amt->insert(new TStaticText(TRect(3,14,6,15)," 4"));
		amt->insert(new TStaticText(TRect(47,17,65,18),"Injection Ratio"));

		TView *b=new TInputLine(TRect(11,8,21,9),NUMLINE);
		amt->insert(b);

		TView *f=new TInputLine(TRect(23,8,33,9),NUMLINE);
		amt->insert(f);

		TView *j=new TInputLine(TRect(37,8,47,9),NUMLINE);
		amt->insert(j);

		TView *n=new TInputLine(TRect(53,8,63,9),NUMLINE);
		amt->insert(n);

		TView *c=new TInputLine(TRect(11,10,21,11),NUMLINE);
		amt->insert(c);

		TView *g=new TInputLine(TRect(23,10,33,11),NUMLINE);
		amt->insert(g);

		TView *k=new TInputLine(TRect(37,10,47,11),NUMLINE);
		amt->insert(k);

		TView *o=new TInputLine(TRect(53,10,63,11),NUMLINE);
		amt->insert(o);

		TView *d=new TInputLine(TRect(11,12,21,13),NUMLINE);
		amt->insert(d);

		TView *h=new TInputLine(TRect(23,12,33,13),NUMLINE);
		amt->insert(h);

		TView *l=new TInputLine(TRect(37,12,47,13),NUMLINE);
		amt->insert(l);

		TView *p=new TInputLine(TRect(53,12,63,13),NUMLINE);
		amt->insert(p);

		TView *e=new TInputLine(TRect(11,14,21,15),NUMLINE);
		amt->insert(e);

		TView *i=new TInputLine(TRect(23,14,33,15),NUMLINE);
		amt->insert(i);

		TView *m=new TInputLine(TRect(37,14,47,15),NUMLINE);
		amt->insert(m);

		TView *q=new TInputLine(TRect(53,14,63,15),NUMLINE);
		amt->insert(q);


		amt->insert(new TButton(TRect(10,17,20,19),"~O~k",cmOK,bfDefault));
		amt->insert(new TButton(TRect(25,17,35,19),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TRadioButtons(TRect(45,18,65,20),
			new TSItem("Time Basis",
			new TSItem("Volume Basis",0)));
		amt->insert(a);


		amt->setData(Inj1RateData);

		ushort control=deskTop->execView(amt);

		if (control!=cmCancel){
			amt->getData(Inj1RateData);
			strcpy(lwgen,"'Y'");
			}
		}
	destroy(amt);
	double trash;

	for (i=0,error=0;i<4;i++){
		for (int j=0;j<2;j++){
			if (verify(Inj1RateData->data[i][j],&injwellrate[0][i][j]))
				error=1;
			}
		}
	for (i=0,intstor[5]=0;i<4;i++){
		if (verify(Inj1RateData->data[i][3],&injamount[i][0]))
			error=1;
		if (verify(Inj1RateData->data[i][2],&injamount[i][1]))
			error=1;
		if (injamount[i][0]>0)
			intstor[5]++;
		}
//Convert WAG to TMORVL

	for (i=0;i<4;i++){
		injamount[i][1]=1-1/(injamount[i][1]+1);
		if (injwellrate[0][i][0]>0){
			if (!injwellrate[0][i][1]>0)
				injamount[i][1]=1.0;
			}
		else{
			injamount[i][1]=0.0;
			}
		}


	for (i=1;i<intstor[1];i++){
		sprintf(display,"Injection Well #%d     X=%-10.2lf  Y=%-10.2lf",
			i+1,injwellstor[i][0],injwellstor[i][1]);


		for (int jj=0;jj<4;jj++){
			if (jj==0){
				if	(injwellrate[i][jj][0]>0)
					sprintf(Inj2RateData->data[7],"%.2lf",injwellrate[i][jj][0]);
				else
					Inj2RateData->data[7][0]='\0';
				}
			else{
				if (injwellrate[i][jj][0]>0)
					sprintf(Inj2RateData->data[jj*2-1],"%.2lf",injwellrate[i][jj][0]);
				else
					Inj2RateData->data[jj*2-1][0]='\0';
				}
			if (injwellrate[i][jj][1]>0)
				sprintf(Inj2RateData->data[jj*2],"%.2lf",injwellrate[i][jj][1]);
			else
				Inj2RateData->data[jj*2][0]='\0';
			}

		TDialog *amt2=new TDialog(TRect(5,1,75,22),"Custom Pattern Allocated Rates");
		if (amt2){
			amt2->insert(new TStaticText(TRect(1,1,68,2),display));
			amt2->insert(new TStaticText(TRect(5,3,68,4),"         Allocated Rates"
				"                     Amount Injected"));
			amt2->insert(new TStaticText(TRect(5,4,68,5),"      ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
				"ÍÍÍ    Water/CO2   Water (Waterflood)"));
			amt2->insert(new TStaticText(TRect(5,5,68,6),"        Water        CO2  "
				"      Injection   or CO2 (CO2 or WAG)"));
			amt2->insert(new TStaticText(TRect(2,6,68,7),"Period    bbls/day   MMSCF/"
				"day       Ratio            HCPV"));
			amt2->insert(new TStaticText(TRect(2,7,68,8),"ÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄ  ÄÄÄÄÄÄ"
				"ÄÄÄÄ    ÄÄÄÄÄÄÄÄÄÄ   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"));

			amt2->insert(new TStaticText(TRect(3,8,6,9)," 1"));
			if (intstor[5]>1)
				amt2->insert(new TStaticText(TRect(3,10,6,11)," 2"));
			if (intstor[5]>2)
				amt2->insert(new TStaticText(TRect(3,12,6,13)," 3"));
			if (intstor[5]>3)
				amt2->insert(new TStaticText(TRect(3,14,6,15)," 4"));
			amt2->insert(new TStaticText(TRect(47,17,65,18),"Injection Ratio"));
			if (Inj1RateData->box==0)
				amt2->insert(new TStaticText(TRect(47,18,65,19),"Time Basis"));
			else
				amt2->insert(new TStaticText(TRect(47,18,65,19),"Volume Basis"));

			for (int jj=0;jj<intstor[5];jj++){

				}


			TView *f=new TInputLine(TRect(23,8,33,9),NUMLINE);
			amt2->insert(f);

			TView *c=new TInputLine(TRect(11,10,21,11),NUMLINE);
			amt2->insert(c);

			TView *g=new TInputLine(TRect(23,10,33,11),NUMLINE);
			amt2->insert(g);

			TView *d=new TInputLine(TRect(11,12,21,13),NUMLINE);
			amt2->insert(d);

			TView *h=new TInputLine(TRect(23,12,33,13),NUMLINE);
			amt2->insert(h);

			TView *e=new TInputLine(TRect(11,14,21,15),NUMLINE);
			amt2->insert(e);

			TView *i=new TInputLine(TRect(23,14,33,15),NUMLINE);
			amt2->insert(i);


			amt2->insert(new TButton(TRect(10,17,20,19),"~O~k",cmOK,bfDefault));
			amt2->insert(new TButton(TRect(25,17,35,19),"~C~ancel",cmCancel,bfNormal));

			TView *b=new TInputLine(TRect(11,8,21,9),NUMLINE);
			amt2->insert(b);

			amt2->setData(Inj2RateData);

			ushort control=deskTop->execView(amt2);

			if (control!=cmCancel){
				amt2->getData(Inj2RateData);
				}
			}
		destroy(amt2);
		double trash;

		for (jj=0;jj<4;jj++){
			if (jj==0){
				if (verify(Inj2RateData->data[7],&injwellrate[i][jj][0]))
					error=1;
				}
			else{
				if (verify(Inj2RateData->data[jj*2-1],&injwellrate[i][jj][0]))
					error=1;
				}
			if (verify(Inj2RateData->data[jj*2],&injwellrate[i][jj][1]))
				error=1;
				}
		}
	if (Inj1RateData->box==0)
		strcpy(wagtag,"'T'");
	else
		strcpy(wagtag,"'V'");
	if (error){
		Mistake();
		InjRateDialog();
		}

	}

//***************************
// Calculate OOIP dialog
//***************************
void TPredApp::CalcOoipDialog()
{
	TDialog *cop=new TDialog(TRect(20,2,60,17),"Calculate OOIP");

	cop->helpCtx=1000;
	if (cop){
		cop->insert(new TStaticText(TRect(3,8,38,11),
		"If a custom pattern is specified\n"
		"the area will be calculated from\n"
		"the custom pattern boundaries"));



		TView *a=new TInputLine(TRect(14,4,24,5),NUMLINE);
		cop->insert(a);
		cop->insert(new TLabel(TRect(3,4,13,5),"Thickness",a));
		cop->insert(new TLabel(TRect(24,4,38,5),"feet",a));

		TView *b=new TInputLine(TRect(14,6,24,7),NUMLINE);
		cop->insert(b);
		cop->insert(new TLabel(TRect(3,6,13,7),"Porosity",b));
		cop->insert(new TLabel(TRect(24,6,38,7),"fraction",b));

		cop->insert(new TButton(TRect(7,12,17,14),"~O~k",cmOK,bfDefault));
		cop->insert(new TButton(TRect(22,12,32,14),"~C~ancel",cmCancel,bfNormal));

		TView *f=new TInputLine(TRect(14,2,24,3),NUMLINE);
		cop->insert(f);
		cop->insert(new TLabel(TRect(3,2,13,3),"Area",f));
		cop->insert(new TLabel(TRect(24,2,38,3),"acres",f));

		cop->setData(CalcOoipData);

		ushort control=deskTop->execView(cop);

		if (control!=cmCancel){
			cop->getData(CalcOoipData);
			ooipflag=0;
			}
		}

	destroy(cop);
	int error=0;

	if (verify(CalcOoipData->data[0],&datastor[35]))
		error=1;
	if (verify(CalcOoipData->data[1],&datastor[36]))
		error=1;
	if (verify(CalcOoipData->data[2],&datastor[34]))
		error=1;
	datastor[34]*=43560;		// convert to square feet

	if (error){
		Mistake();
		CalcOoipDialog();
		}
}



//*******************************
// Calc final stuff and Write the data file
//*******************************
void TPredApp::WriteFile(char *fp){
// put the stuff for calculation here before final output
	char space[11];
	strcpy(space,",      ");
	double injsum=0;
	double injmax=0;
	double prodsum=0;
	double x[11];
	double y[11];

	ofstream fo(fp);
	if (pat[1]=='C'){
		for (int ii=0;ii<intstor[2];ii++){
			x[ii]=boundstor[ii][0];
			y[ii]=boundstor[ii][1];
			}
		datastor[34]=arpoly(intstor[2],x,y);
		}
	if (ooipflag){
		datastor[36]=.2;		//Imaginary porosity
		datastor[35]=ooip*datastor[4]*5.615/
			(datastor[34]*datastor[36]*(1-datastor[15]));
		}
	fo<<"'"<<TitleData->title<<"'"<<endl;
	fo<<"'********** WELL AND PATTERN DATA ********'"<<endl;
	fo<<"'PATTERN'"<<endl;
	fo<<pat<<endl;
	fo<<"'NWELLS     NOINJ'"<<endl;
	if(pat[1]=='C'){
		fo<<intstor[0]<<space<<intstor[1]<<endl;
		fo<<"'WELLS     WELLY     WELLQ'"<<endl;
		for (int i=0;i<intstor[1];i++){
			for (int jj=0;jj<4;jj++)
				if (injwellrate[i][jj][0]>injmax)
					injmax=injwellrate[i][jj][0];
			}
		if (injmax<0.0000001){
			for (int i=0;i<intstor[1];i++)
				for (int jj=0;jj<4;jj++)
					if (injwellrate[i][jj][1]>injmax)
						injmax=injwellrate[i][jj][1];
			for (i=0;i<intstor[1];i++)
				for (int jj=0;jj<4;jj++)
					if (injwellrate[i][jj][1]>injwellq[i])
						injwellq[i]=injwellrate[i][jj][1];
			}
		else
			for (i=0;i<intstor[1];i++)
				for (int jj=0;jj<4;jj++)
					if (injwellrate[i][jj][0]>injwellq[i])
						injwellq[i]=injwellrate[i][jj][0];
		for (i=0;i<intstor[1];i++){
			injwellq[i]=injwellq[i]/injmax;
			injsum+=injwellq[i];
			}
		for (i=0;i<intstor[0]-intstor[1];i++){
			prodsum+=prodwellq[i];
			}
		injmax=injsum/prodsum;
		for (i=0;i<intstor[0]-intstor[1];i++)
			prodwellq[i]=prodwellq[i]*-injmax;
		for (i=0;i<intstor[1];i++)
			fo<<injwellstor[i][0]<<space<<injwellstor[i][1]<<space<<injwellq[i]<<endl;
		for (i=0;i<intstor[0]-intstor[1];i++)
			fo<<prodwellstor[i][0]<<space<<prodwellstor[i][1]<<space<<prodwellq[i]<<endl;
		fo<<"'NBNDPT'"<<endl;
		fo<<intstor[2]<<endl;
		fo<<"'BOUNDX     BOUNDY'"<<endl;
		for (i=0;i<intstor[2];i++)
			fo<<boundstor[i][0]<<space<<boundstor[i][1]<<endl;
		}
	else{
		switch (pat[1]){
			case '5':
				fo<<"2"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -1"<<endl;
				break;
			case 'L':
				fo<<"2"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -1"<<endl;
				break;
			case '2':
				fo<<"2"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -1"<<endl;
				break;
			case '4':
				fo<<"3"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -.5"<<endl;
				fo<<"0,     1,    -.5"<<endl;
				break;
			case '9':
				fo<<"3"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -.5"<<endl;
				fo<<"0,     1,    -.5"<<endl;
				break;
			case '7':
				fo<<"3"<<space<<"1"<<endl;
				fo<<"'WELLS     WELLY     WELLQ'"<<endl;
				fo<<"0,     0,     1"<<endl;
				fo<<"1,     1,    -.5"<<endl;
				fo<<"0,     1,    -.5"<<endl;
				break;
			}
 		fo<<"'NBNDPT'"<<endl;
		fo<<"5"<<endl;
		fo<<"'BOUNDX     BOUNDY'"<<endl;
		fo<<"0,   0"<<endl;
		fo<<"0,   1"<<endl;
		fo<<"1,   1"<<endl;
		fo<<"1,   0"<<endl;
		fo<<"0,   0"<<endl;
		}
	fo<<"'*************  PROGRAM CONTROLS  ********'"<<endl;
	fo<<"'LWGEN     OUTTIM'"<<endl;
	fo<<lwgen<<space<<datastor[27]<<endl;
	fo<<"'**** RELATIVE PERMEABILITY PARAMETERS ***'"<<endl;
	fo<<"'SORW     SORG     SORM'"<<endl;
	fo<<datastor[12]<<space<<datastor[22]<<space<<datastor[24]<<endl;
	fo<<"'SGR     SSR'"<<endl;
	fo<<datastor[18]<<space<<datastor[23]<<endl;
	fo<<"'SWC     SWIR'"<<endl;
	fo<<datastor[15]<<space<<datastor[11]<<endl;
	fo<<"'KROCW     KWRO     KRSMAX     KRGCW'"<<endl;
	fo<<datastor[17]<<space<<datastor[14]<<space<<datastor[26];
	fo<<space<<datastor[20]<<endl;
	fo<<"'EXPOW    EXPW     EXPS     EXPG     EXPOG'"<<endl;
	fo<<datastor[16]<<space<<datastor[13]<<space<<datastor[25];
	fo<<space<<datastor[19]<<space<<datastor[21]<<endl;
	fo<<"'KRMSEL     W'"<<endl;
	fo<<intstor[3]<<space<<datastor[28]<<endl;
	fo<<"'***************  FLUID DATA  ************'"<<endl;
	fo<<"'VISO     VISW'"<<endl;
	fo<<datastor[3]<<space<<datastor[8]<<endl;
	fo<<"'BO       RS       API       SALN       CSG'"<<endl;
	fo<<datastor[4]<<space<<datastor[10]<<space<<datastor[6]<<space;
	fo<<datastor[9]<<space<<datastor[7]<<endl;
	fo<<"'************** RESERVOIR DATA ***********'"<<endl;
	fo<<"'TRES     P     MMP'"<<endl;
	fo<<datastor[0]<<space<<datastor[1]<<space<<datastor[2]<<endl;
	fo<<"'DPCOEF   PERMAV   THICK   POROS   NLAYERS'"<<endl;
	fo<<datastor[5]<<space<<datastor[32]<<space;
	fo<<datastor[35]<<space<<datastor[36]<<space<<intstor[4]<<endl;
	fo<<"'SOINIT     SGINIT     SWINIT'"<<endl;
	fo<<datastor[29]<<space<<datastor[30]<<space<<datastor[31]<<endl;
	fo<<"'AREA     XKVH'"<<endl;
	fo<<datastor[34]<<space<<datastor[33]<<endl;
	fo<<"'********** INJECTION PARAMETERS *********'"<<endl;
	fo<<"'NTIMES     WAGTAG'"<<endl;
	fo<<intstor[5]<<space<<wagtag<<endl;
	fo<<"'HCPVI    WTRRAT    SOLRAT     TMORVL'"<<endl;
	for (int i=0;i<intstor[1];i++){
		for (int j=0;j<intstor[5];j++){
			fo<<injamount[j][0]<<space<<injwellrate[i][j][0]<<space;
			fo<<injwellrate[i][j][1]<<space<<injamount[j][1]<<endl;
			}
		}

  	fo.close();
	}


//*******************************
// Read File Dialog
//*******************************
int TPredApp::ReadFile(){
	char filename[MAXPATH];
	char trash[80];
	char *comma;
	double temp;
	int error=0;

	TFileDialog *f=new TFileDialog("*.sav","Data Files","",fdOpenButton,25);
	if (f!=0 && deskTop->execView(f)!=cmCancel){
		f->getFileName(filename);
		}
	destroy(f);
	ifstream fi(filename);
	fi.getline(trash,54,'\n');
	comma=trash;
	comma++;
	comma[strlen(comma)-1]='\0';
	strcpy(TitleData->title,comma);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(pat,80,'\n');
	switch (pat[1]){
		case '5':
			PatternData->box=0;
			break;
		case '7':
			PatternData->box=1;
			break;
		case '9':
			PatternData->box=2;
			break;
		case 'L':
			PatternData->box=3;
			break;
		case '4':
			PatternData->box=4;
			break;
		case '2':
			PatternData->box=5;
			break;
		}
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%d, %d",&intstor[0],&intstor[1]);
	fi.getline(trash,80,'\n');
	for (int i=0;i<intstor[1];i++){
		fi.getline(trash,80,'\n');
		sscanf(trash,"%lf, %lf, %lf",
			&injwellstor[i][0],&injwellstor[i][1],&injwellq[i]);
		if (i==0)
			sprintf(InjWellData->data[19],"%.1lf",injwellstor[0][0]);
		else
			sprintf(InjWellData->data[2*i-1],"%.1lf",injwellstor[i][0]);
		sprintf(InjWellData->data[2*i],"%.1lf",injwellstor[i][1]);
		}
	for (i=0;i<intstor[0]-intstor[1];i++){
		fi.getline(trash,80,'\n');
		sscanf(trash,"%lf, %lf, %lf",
			&prodwellstor[i][0],&prodwellstor[i][1],&prodwellq[i]);
		if (i==0)
			sprintf(ProdWellData->data[19],"%.1lf",prodwellstor[0][0]);
		else
			sprintf(ProdWellData->data[2*i-1],"%.1lf",prodwellstor[i][0]);
		sprintf(ProdWellData->data[2*i],"%.1lf",prodwellstor[i][1]);
		}
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%d",&intstor[2]);
	fi.getline(trash,80,'\n');
	for (i=0;i<intstor[2]-1;i++){
		fi.getline(trash,80,'\n');
		sscanf(trash,"%lf, %lf",&boundstor[i][0],&boundstor[i][1]);
		if (i==0)
			sprintf(BoundData->data[19],"%.1lf",boundstor[0][0]);
		else
			sprintf(BoundData->data[2*i-1],"%.1lf",boundstor[i][0]);
		sprintf(BoundData->data[2*i],"%.1lf",boundstor[i][1]);
		}
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(lwgen,80,',');
	fi.get();
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf",&datastor[27]);
	if (datastor[27]>.75)		//Annual
		ReportData->box=0;
	else
		if (datastor[27]>.4)		//SemiAnnual
			ReportData->box=1;
		else
			if (datastor[27]>.2)	  //Quarterly
				ReportData->box=2;
			else
				ReportData->box=3;  //Monthly
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf",&datastor[12],&datastor[22],&datastor[24]);
	sprintf(KrwData->data[1],"%.4lf",datastor[12]);
	sprintf(KrogData->data[1],"%.4lf",datastor[22]);
	sprintf(KrsData->data,"%.4lf",datastor[24]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf",&datastor[18],&datastor[23]);
	sprintf(KrgData->data[0],"%.4lf",datastor[18]);
	sprintf(Krs1Data->data[0],"%.4lf",datastor[23]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf",&datastor[15],&datastor[11]);
	sprintf(KrowData->data[0],"%.4lf",datastor[15]);
	sprintf(KrwData->data[0],"%.4lf",datastor[11]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf, %lf",
		&datastor[17],&datastor[14],&datastor[26],&datastor[20]);
	sprintf(KrowData->data[2],"%.4lf",datastor[17]);
	sprintf(KrwData->data[3],"%.4lf",datastor[14]);
	sprintf(Krs1Data->data[3],"%.4lf",datastor[26]);
	sprintf(KrgData->data[2],"%.4lf",datastor[20]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf, %lf, %lf",
		&datastor[16],&datastor[13],&datastor[25],&datastor[19],&datastor[21]);
	sprintf(KrowData->data[1],"%.4lf",datastor[16]);
	sprintf(KrwData->data[2],"%.4lf",datastor[13]);
	sprintf(Krs1Data->data[2],"%.4lf",datastor[25]);
	sprintf(KrgData->data[1],"%.4lf",datastor[19]);
	sprintf(KrogData->data[0],"%.4lf",datastor[21]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%d, %lf",&intstor[3],&datastor[28]);
	KrmData->box=intstor[3];
	sprintf(OmegaData->data,"%.4lf",datastor[28]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf",&datastor[3],&datastor[8]);
	sprintf(FluidData1->data[3],"%.3lf",datastor[3]);
	sprintf(FluidData2->data[2],"%.3lf",datastor[8]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf, %lf, %lf",
		&datastor[4],&datastor[10],&datastor[6],&datastor[9],&datastor[7]);
	sprintf(FluidData1->data[4],"%.2lf",datastor[4]);
	sprintf(FluidData2->data[4],"%.2lf",datastor[10]);
	sprintf(FluidData2->data[0],"%.2lf",datastor[6]);
	sprintf(FluidData2->data[3],"%.0lf",datastor[9]);
	sprintf(FluidData2->data[1],"%.3lf",datastor[7]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf",&datastor[0],&datastor[1],&datastor[2]);
	sprintf(FluidData1->data[0],"%.2lf",datastor[0]);
	sprintf(FluidData1->data[1],"%.2lf",datastor[1]);
	sprintf(FluidData1->data[2],"%.2lf",datastor[2]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf, %lf, %d",
		&datastor[5],&datastor[32],&datastor[35],&datastor[36],&intstor[4]);
	sprintf(FluidData1->data[5],"%.3lf",datastor[5]);
	sprintf(CalcOoipData->data[0],"%.2lf",datastor[35]);
	sprintf(CalcOoipData->data[1],"%.3lf",datastor[36]);
	sprintf(LayerData->data,"%d",intstor[4]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf, %lf",&datastor[29],&datastor[30],&datastor[31]);
	sprintf(SoiData->data,"%.4lf",datastor[29]);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	sscanf(trash,"%lf, %lf",&datastor[34],&datastor[33]);
	sprintf(CalcOoipData->data[2],"%.2lf",datastor[34]/43560);
	fi.getline(trash,80,'\n');
	fi.getline(trash,80,'\n');
	fi>>intstor[5];
	fi.getline(trash,80,'\'');
	fi.putback('\'');
	fi.getline(wagtag,80,'\n');
	if (wagtag[1]=='T'){
		PAmtData->box=0;
		Inj1RateData->box=0;
		}
	else{
		PAmtData->box=1;
		Inj1RateData->box=1;
		}
	fi.getline(trash,80,'\n');
	for (i=0;i<intstor[1];i++){
		for (int j=0;j<intstor[5];j++){
			fi.getline(trash,80,'\n');
			sscanf(trash,"%lf, %lf, %lf, %lf",
				&injamount[j][0],&injwellrate[i][j][0],
				&injwellrate[i][j][1],&injamount[j][1]);
			}
		}
	fi.close();
	return error;
	}



//*******************************
// Save File Dialog
//*******************************
int TPredApp::SaveFile(){
	char filename[MAXPATH];
	int error=0;

	TFileDialog *f=new TFileDialog("*.sav","Save Data File","",fdOKButton,0);
	if (f!=0 && deskTop->execView(f)!=cmCancel){
		f->getFileName(filename);
		}
	destroy(f);
	WriteFile(filename);
	return error;
}


//*******************************
// Title Dialog
//*******************************
void TPredApp::TitleDialog()
{
	TDialog *tit=new TDialog(TRect(7,6,74,18),"Project Title");

	tit->helpCtx=1000;
	if (tit){

		tit->insert(new TStaticText(TRect(21,3,50,4),"Enter the Project Title"));
		tit->insert(new TButton(TRect(20,8,30,10),"~O~k",cmOK,bfDefault));
		tit->insert(new TButton(TRect(34,8,44,10),"~C~ancel",cmCancel,bfNormal));

		TView *d=new TInputLine(TRect(5,5,60,6),55);
		tit->insert(d);

		tit->setData(TitleData);

		ushort control=deskTop->execView(tit);

		if (control!=cmCancel){
			tit->getData(TitleData);
			}
		}
	destroy(tit);
}




//***************************
// Production Well Rates Dialog
//***************************
void TPredApp::ProdWellRates()
{
	char trash[20];

	TDialog *pwr=new TDialog(TRect(1,2,79,20),"Allocated Production Rates");

	pwr->helpCtx=1000;
	if (pwr){

		pwr->insert(new TStaticText(TRect(24,2,40,3),"Allocated"));
		pwr->insert(new TStaticText(TRect(62,2,75,3),"Allocated"));
		pwr->insert(new TStaticText(TRect(7,3,40,4),"X         Y      "
			"Fluid Prod."));
		pwr->insert(new TStaticText(TRect(45,3,75,4),"X         Y      "
			"Fluid Prod."));

		for (int i=0;i<5;i++){
			if (i==0){
				strcpy(trash,"\003");
				strcat(trash,ProdWellData->data[19]);
				pwr->insert(new TStaticText(TRect(2,2*i+5,12,2*i+6),trash));
				}
			else{
				strcpy(trash,"\003");
				strcat(trash,ProdWellData->data[2*i-1]);
				pwr->insert(new TStaticText(TRect(2,2*i+5,12,2*i+6),trash));
				}
			strcpy(trash,"\003");
			strcat(trash,ProdWellData->data[2*i]);
			pwr->insert(new TStaticText(TRect(13,2*i+5,23,2*i+6),trash));

			}
		for (i=5;i<10;i++){
			strcpy(trash,"\003");
			strcat(trash,ProdWellData->data[2*i-1]);
			pwr->insert(new TStaticText(TRect(40,2*i-5,50,2*i-4),trash));
			strcpy(trash,"\003");
			strcat(trash,ProdWellData->data[2*i]);
			pwr->insert(new TStaticText(TRect(51,2*i-5,61,2*i-4),trash));
			}

		TView *b=new TInputLine(TRect(24,7,34,8),NUMLINE);
		pwr->insert(b);

		TView *c=new TInputLine(TRect(24,9,34,10),NUMLINE);
		pwr->insert(c);

		TView *d=new TInputLine(TRect(24,11,34,12),NUMLINE);
		pwr->insert(d);

		TView *e=new TInputLine(TRect(24,13,34,14),NUMLINE);
		pwr->insert(e);

		TView *f=new TInputLine(TRect(62,5,72,6),NUMLINE);
		pwr->insert(f);

		TView *g=new TInputLine(TRect(62,7,72,8),NUMLINE);
		pwr->insert(g);

		TView *h=new TInputLine(TRect(62,9,72,10),NUMLINE);
		pwr->insert(h);

		TView *j=new TInputLine(TRect(62,11,72,12),NUMLINE);
		pwr->insert(j);

		TView *k=new TInputLine(TRect(62,13,72,14),NUMLINE);
		pwr->insert(k);

		pwr->insert(new TButton(TRect(25,15,35,17),"~O~k",cmOK,bfDefault));
		pwr->insert(new TButton(TRect(45,15,55,17),"~C~ancel",cmCancel,bfNormal));

		TView *a=new TInputLine(TRect(24,5,34,6),NUMLINE);
		pwr->insert(a);


		pwr->setData(ProdRateData);

		ushort control=deskTop->execView(pwr);

		if (control!=cmCancel){
			pwr->getData(ProdRateData);
			strcpy(lwgen,"'Y'");
			}
		}

	destroy(pwr);
	int error=0;

	for (int i=0;i<9;i++){
		if (verify(ProdRateData->data[i],&prodwellq[i+1]))
			error=1;
		}
	if (verify(ProdRateData->data[9],&prodwellq[0]))
		error=1;


	if (error){
		Mistake();
		ProdWellRates();
		}
}

//******************************
// View the error file
//******************************

void TPredApp::ViewError()
{
	TDialog *err=new TDialog(TRect(1,1,79,22), "Errors");

	if (err){
		for (int i=0;i<15;i++)
			err->insert(new TStaticText(TRect(3,i+1,75,i+2),lines[i]));
		err->insert(new TButton(TRect(35,17,45,19),"~O~k",cmOK,bfDefault));
		deskTop->execView(err);
		}
	destroy(err);
}



//***************************
// RadioButtons to Plot the Streamtubes
//***************************
void TPredApp::StreamDialog()
{
	TDialog *p=new TDialog(TRect(24,5,56,13),"Streamtubes");
	p->helpCtx=1000;
	if (p){

		p->insert(new TButton(TRect(18,5,28,7),"~C~ancel",cmCancel,bfNormal));
		p->insert(new TButton(TRect(4,5,14,7),"~O~K",cmOK,bfDefault));

		TView *a=new TRadioButtons(TRect(5,2,27,4),
			new TSItem("Don't Plot",
			new TSItem("Plot Streamtubes",0)));
		p->insert(a);

		p->setData(StreamData);
		ushort control=deskTop->execView(p);
		if (control!=cmCancel)
			p->getData(StreamData);
		}
	destroy(p);
	switch (StreamData->box)
	{
		case 0:
			strcpy(lwgen,"'N'");
			break;
		case 1:
			strcpy(lwgen,"'P'");
			break;
		default:
			strcpy(lwgen,"'N'");
		}
}


//*******************************
// Loop to handle the events from the menu
//*******************************

void TPredApp::handleEvent( TEvent& event )
{
  TApplication::handleEvent( event );
  if( event.what == evCommand )
	  {
	  switch( event.message.command )
		  {
			case AboutCmd:
				clearEvent(event);
				break;
			case PatternCmd:
				PatternDialog();
				clearEvent(event);
				break;
			case FluidsCmd:
				FluidDialog1();
				FluidDialog2();
				clearEvent(event);
				break;
			case KrwCmd:
				KrwDialog();
				clearEvent(event);
				break;
			case KrowCmd:
				KrowDialog();
				clearEvent(event);
				break;
			case KrgCmd:
				KrgDialog();
				clearEvent(event);
				break;
			case KrogCmd:
				KrogDialog();
				clearEvent(event);
				break;
			case KrsCmd:
				KrsDialog();
				clearEvent(event);
				break;
			case SatCmd:
				KrwDialog();
				KrowDialog();
				KrgDialog();
				KrogDialog();
				KrsDialog();
				SoiDialog();
				clearEvent(event);
				break;
			case DoitCmd:
				while (strlen(TitleData->title)<1){
					TitleDialog();
					}
				TPredApp::suspend();
				unlink("error.in");
				if (pat[1]=='C' && lwgen[1]=='Y'){	// LWGEN==Y means second pass
					WriteFile("indata");
					spawnl(P_WAIT,"co2.exe",NULL);
//					EXECUTE the simulator
					}
				if (lwgen[1]=='P'){
					if (pat[1]=='C'){
						WriteFile("indata");
						spawnl(P_WAIT,"co2.exe",NULL);
//						EXECUTE the simulator
						}
					else
						lwgen[1]='N';
					}
				lwgen[1]='N';
				TPredApp::resume();
				TPredApp::redraw();
				int handle=open("error.in",O_RDONLY);
				long fl=filelength(handle);
				close(handle);
				if (fl>10){
					WriteFile("indata");
					readFile("error.in");
					ViewError();
					deleteFile();
					clearEvent(event);
					break;
					}
				TPredApp::suspend();
				WriteFile("indata.");
				spawnl(P_WAIT,"co2.exe",NULL);
				TPredApp::resume();
				TPredApp::redraw();
				WriteFile("indata");
				handle=open("error.in",O_RDONLY);
				fl=filelength(handle);
				close(handle);
				if (fl>10){
					readFile("error.in");
					ViewError();
					deleteFile();
					}
				clearEvent(event);
				break;
			case FileCmd:
				ReadFile();
				clearEvent(event);
				break;
			case PAmountsCmd:
				PAmtDialog();
				clearEvent(event);
				break;
			case Krs1Cmd:
				Krs1Dialog();
				clearEvent(event);
				break;
			case OoipCmd:
				OoipDialog();
				clearEvent(event);
				break;
			case FreqCmd:
				ReportDialog();
				clearEvent(event);
				break;
			case OmegaCmd:
				OmegaDialog();
				clearEvent(event);
				break;
			case LayerCmd:
				LayerDialog();
				clearEvent(event);
				break;
			case KrmCmd:
				KrmDialog();
				clearEvent(event);
				break;
			case SoiCmd:
				SoiDialog();
				clearEvent(event);
				break;
			case CBoundCmd:
				BoundDialog();
				clearEvent(event);
				break;
			case CLocateCmd:
				InjWellDialog();
				ProdWellDialog();
				clearEvent(event);
				break;
			case InjectionCmd:
				InjRateDialog();
				clearEvent(event);
				break;
			case CalcOoipCmd:
				CalcOoipDialog();
				clearEvent(event);
				break;
			case SaveFileCmd:
				SaveFile();
				clearEvent(event);
				break;
			case DoPatternCmd:
				BoundDialog();
				InjWellDialog();
				ProdWellDialog();
				InjRateDialog();
				ProdWellRates();
				clearEvent(event);
				break;
			case ProdCmd:
				ProdWellRates();
				clearEvent(event);
				break;
			case TitleCmd:
				TitleDialog();
				clearEvent(event);
				break;
			case PlotCmd:
				StreamDialog();
				clearEvent(event);
				break;
	     	default:
				break;
			}
  }
}

//*******************************
// Menu bar for the application
//*******************************

TMenuBar *TPredApp::initMenuBar( TRect r )
{

	r.b.y = r.a.y+1;

	return new TMenuBar( r,
		*new TSubMenu( /*"~\xF0~"*/"Prophet",0 ) +
			*new TMenuItem( "About",AboutCmd,kbAltB,hcNoContext, "Alt-b" )+
		*new TSubMenu( "~F~ile",kbAltF)+
			*new TMenuItem( "Read",FileCmd, kbAltR, hcNoContext,"")+
			*new TMenuItem( "Save",SaveFileCmd, 0, hcNoContext,"")+
			newLine()+
			*new TMenuItem( "E~x~it",cmQuit, cmQuit, hcNoContext, "Alt-X" )+
		*new TSubMenu( "~D~ata",kbAltD )+
			*new TMenuItem( "Reservoir" , FluidsCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Specify OOIP",OoipCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Calculate OOIP",CalcOoipCmd, 0 , hcNoContext, "")+
		*new TSubMenu( "~S~aturations",kbAltS)+
			*new TMenuItem( "Do It All",SatCmd, 0, hcNoContext, "")+
			newLine()+
			*new TMenuItem( "Krw",KrwCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Krow",KrowCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Krg",KrgCmd, 0 , hcNoContext, "")+
			*new TMenuItem( "Krog",KrogCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Krs",KrsCmd, 0, hcNoContext, "")+
			newLine()+
			*new TMenuItem( "Soi",SoiCmd, 0, hcNoContext, "")+
		*new TSubMenu( "~P~re-Set Patterns",kbAltP)+
			*new TMenuItem( "Pattern",PatternCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Amounts and Rates",PAmountsCmd, 0, hcNoContext, "")+
		*new TSubMenu( "~C~ustom Pattern",kbAltC)+
			*new TMenuItem( "Do It All",DoPatternCmd, 0, hcNoContext, "")+
			newLine()+
			*new TMenuItem( "Boundaries",CBoundCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Well Locations",CLocateCmd , 0, hcNoContext, "")+
			*new TMenuItem( "Injection",InjectionCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Production",ProdCmd, 0, hcNoContext, "")+
		*new TSubMenu( "~O~ptions",kbAltO)+
			*new TMenuItem( "Project Title",TitleCmd,0,hcNoContext,"")+
			*new TMenuItem( "Advanced Options",0,
			new TMenu(
			*new TMenuItem("Number of Layers",LayerCmd,0,hcNoContext,"",
			new TMenuItem("Miscible Rel Perm, Krm",KrmCmd,0,hcNoContext,"",
			new TMenuItem("Mixing Parameter, Omega",OmegaCmd,0,hcNoContext,"",
			new TMenuItem("Solvent Rel Perm, Krs",Krs1Cmd,0,hcNoContext,""
			))))), hcNoContext)+
			*new TMenuItem( "Report Frequency",FreqCmd, 0, hcNoContext, "")+
			*new TMenuItem( "Plot Streamlines",PlotCmd, 0, hcNoContext, "")
		);

}


//*******************************
// Status line for the application
//*******************************

TStatusLine *TPredApp::initStatusLine( TRect r )
{
   r.a.y = r.b.y-1;
   return new TStatusLine( r,
      *new TStatusDef( 0, 999 ) +
      	*new TStatusItem( "E~x~it",kbAltX, cmQuit ) +
			*new TStatusItem( "Do ~I~t",kbAltI, DoitCmd)+
      	*new TStatusItem( "~F10~ for Menu",kbF10, cmMenu )+
		*new TStatusDef(1000, 1001)+
			*new TStatusItem( "Esc Exit, Tab between items, or Use Mouse.",0,0)+
		*new TStatusDef(1002,1003)+
			*new TStatusItem("Press ~Esc~ or use mouse button to continue.",0,0)
            );
}


//*******************************
// Replace the idle loop to refresh the status line
//*******************************
void TPredApp::idle(){
	TProgram::statusLine->update();
	}



//*******************************
// Mistake handler
//*******************************
void TPredApp::Mistake(){
	beep();
	return;
	}


//********************************
// Nasty little beep to make sure you know you've goofed
void beep(void){
	sound(880);
	delay(300);
	sound(100);
	delay(250);
	nosound();
	}


//*******************************
//*******************************
// Main
//*******************************
//*******************************

int main()
{
	textmode(C80);
	TPredApp Predict;
	Predict.run();
	return 0;
}

