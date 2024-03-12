/*   Function verify -- takes a string and attempts to return
       a double.  The string is passed throught the char pointer *str
       and the value is passed back through the double pointer *ret.
       An integer error code is returned that points to the character
       in the string that is not correct.  Zero is returned if the
       value is valid.  In the event of an err, non-zero return, *ret
       will have the value zero.  If zero is the actual value then
       the error code returned being non-zero is the only way to detect
       the error.
		
		Warning about the global variable temp.  It will haunt you later.

     Usage:  error=verify(string,&value);

*/
#include <math.h>
#include <string.h>

int i,state,err;
char *temp;

void check();

int verify(char *str,double *ret)
{
   temp=str;
	for (i=0;i<strlen(str);i++)
		if (temp[0]==' ')
			temp++;
	for (i=strlen(temp);i>0;i--)
		if (temp[i-1]==' ')
			temp[i-1]='\0';
	i=state=err=0;
	*ret=0;
   check();
   if (!err) *ret=atof(str);
   return err;
   }

void check()
{
   switch (state){
      case 0:{              /* This is the start, nothing has been by */
         switch (*temp){
            case '-':
            case '+':
               state=1;
               i++;
               temp++;
               check();
               break;
            case 'E':
            case 'e':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               state=2;
               i++;
               temp++;
               check();
               break;
            case '.':
               state=3;
               i++;
               temp++;
               check();
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 1:{               /* Minus or plus */
         switch (*temp){
            case '-':
            case '+':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               state=2;
               i++;
               temp++;
               check();
               break;
            case '.':
               state=3;
               i++;
               temp++;
               check();
               break;
            case 'E':
            case 'e':
               err=++i;
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 2:{                /* Numerical before the decimal */
         switch (*temp){
            case '-':
            case '+':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               i++;
               temp++;
               check();
               break;
            case '.':
               state=3;
               i++;
               temp++;
               check();
               break;
            case 'E':
            case 'e':
               state=4;
               i++;
               temp++;
               check();
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 3:{              /* Decimal or Numerical after decimal */
         switch (*temp){
            case '-':
            case '+':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               i++;
               temp++;
               check();
               break;
            case '.':
               err=++i;
               break;
            case 'E':
            case 'e':
               state=4;
               i++;
               temp++;
               check();
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 4:{             /* Exponential */
         switch (*temp){
            case '-':
            case '+':
               state=5;
               i++;
               temp++;
               check();
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               state=6;
               i++;
               temp++;
               check();
               break;
            case '.':
               err=++i;
               break;
            case 'E':
            case 'e':
               err=++i;
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 5:{                /* Minus or Plus after exponential */
         switch (*temp){
            case '-':
            case '+':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               state=6;
               i++;
               temp++;
               check();
               break;
            case '.':
               err=++i;
               break;
            case 'E':
            case 'e':
               err=++i;
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         break;
         }
      case 6:{              /* Numerical after exponential */
         switch (*temp){
            case '-':
            case '+':
               err=++i;
               break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
               i++;
               temp++;
               check();
               break;
            case '.':
               err=++i;
               break;
            case 'E':
            case 'e':
               err=++i;
               break;
            case '\0':
               break;
            default:
               err=++i;
               break;
            }
         }
      }
   return;
   }

