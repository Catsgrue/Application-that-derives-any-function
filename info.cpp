#include <iostream>
#include <fstream>
#include <stack>
#include <queue>
#include <cctype>
#include <string.h>
#include <math.h>
#include <iomanip>
using namespace std;

struct nod
{
    string info;
    nod *st,*dr;
};

nod *arbore=nullptr;
nod *arbore_dervivat=nullptr;

stack <string> s;
stack <nod*> operanzi;
char q[256][256];
bool eroare=false;
bool gasire=false;
bool gasire_operator_unar=false;
bool gasire_punct=false;
bool gasire_e=false;

int k;

int prioritate(char op)
{
    if(op=='(' || op==')')
        return 0;
    if(op=='+' || op=='-')
        return 1;
    if(op=='*' || op=='/')
        return 2;
    if(op=='^')
        return 3;
    return 4;
}

bool operator_unar(string c)
{
    if(c=="sin")
        return true;
    if(c=="cos")
        return true;
    if(c=="tg")
        return true;
    if(c=="ctg")
        return true;
    if(c=="arcsin")
        return true;
    if(c=="arccos")
        return true;
    if(c=="arctg")
        return true;
    if(c=="arcctg")
        return true;
    if(c=="ln")
        return true;
    if(c=="sqrt")
         return true;
    if(c=="log")
        return true;
    if(c=="lg")
        return true;
    if(c=="~")
        return true;
    return false;
}

void forma_postfixata( char v[][256],int ctexp)
{
    int i;
    for(i=0;i<ctexp;i++)
    {
        if(strcmp("x",v[i])==0 || strchr("0123456789",v[i][0])!=NULL || strcmp("e",v[i])==0)
            strcpy(q[k++],v[i]);
        else
        if(operator_unar(v[i])==true)   
            s.push(v[i]);
        else
        if (strcmp(v[i], "-") == 0 && (i == 0 || strchr("+-*/^(", v[i - 1][0]) != NULL))
            s.push("~");
        else
        if(strchr("+-*/^()",v[i][0])!=NULL)
        {
            if(s.empty() || v[i][0]=='(')
                s.push(v[i]);
            else
                if(v[i][0]==')')
                {
                    while(!s.empty() && strcmp(s.top().c_str(),"(")!=0)
                    {
                        strcpy(q[k++],s.top().c_str());
                        s.pop();
                    }
                    s.pop();
                    if(!s.empty())
                        if(operator_unar(s.top())==true)
                        {
                            strcpy(q[k++],s.top().c_str());
                            s.pop();
                        }
                }
                else
                {
                    while (!s.empty() && prioritate(v[i][0]) <= prioritate(s.top()[0])) 
                    {
                        strcpy(q[k++], s.top().c_str());
                        s.pop();
                    }
                s.push(v[i]);
                }
        }
    }

    while(!s.empty())
    {
        strcpy(q[k++],s.top().c_str());
        s.pop();
    }
}

void creare_vector_de_cuvinte(char s[],char v[][256], int &ctexp)
{
    int l=strlen(s);
    int i,linie,coloana;
    linie=coloana=0;
    for(i=0;i<l;i++)
    {

        if(isdigit(s[i]))
            {
                int j=i;
                while(strchr("+-()*^ /",s[j])==NULL && s[j]!='\0' && !isalpha(s[j]))
                    j++;
                for(int k=i;k<j;k++)
                    v[linie][coloana++]=s[k];
                i=j-1;
            }
        else
            if(isalpha(s[i]) && (coloana==0 || isalpha(v[linie][coloana-1]) && v[linie][coloana-1]!='x'))
                v[linie][coloana++]=s[i];
        else
            if(isalpha(s[i]))
            {
                if(coloana>0)
                {
                    v[linie][coloana]='\0';
                    linie++;
                    coloana=0;
                }
                v[linie][coloana]=s[i];
            }
            else
               {
                    if(coloana>0)
                    {
                        v[linie][coloana]='\0';
                        linie++;
                        coloana=0;
                    }
                    v[linie][0]=s[i];
                    v[linie][1]='\0';
                    linie++;
               }
    }
    if(coloana>0)
    {
        v[linie][coloana]='\0';
        linie++;
    }
    ctexp=linie;
}

int verificare_expresie(char v[][256], int ctexp)
{
    int i, k1 = 0, k2 = 0, sum = 0;
    int cnt;

    for (i = 0; i < ctexp - 1; i++)
    {
        if (v[i][0] == '(')
        {
            k1++; sum++;
        }
        else
            if (v[i][0] == ')')
            {
                k2++; sum--;
            }
        if (sum < 0) return 0;
        if (v[i][0] == '(' && strchr("^*/", v[i + 1][0]))
            return 0;
        if (strchr("+-*/^", v[i][0]) && strchr("+-*^/", v[i + 1][0]))
            return 0;
        if (v[i - 1][0] == '(' && v[i][0] == ')')
            return 0;
        if (operator_unar(v[i]) == false && strlen(v[i]) >= 2 && v[i][0] != '-' && !isdigit(v[i][0]))
            return 0;
        if ((isalpha(v[i][0]) && isalpha(v[i + 1][0])) || (isalpha(v[i][0]) && isdigit(v[i + 1][0])) || (isdigit(v[i][0]) && isalpha(v[i + 1][0])))
            return 0;
        if ((isalnum(v[i][0]) && v[i + 1][0] == '(' && operator_unar(v[i]) == false) || (isalnum(v[i + 1][0]) && v[i][0] == ')' && operator_unar(v[i + 1]) == false))
            return 0;
        if (operator_unar(v[i]) == true && v[i + 1][0] != '(' && v[i + 1][0] != '\0')
            return 0;

        cnt = 0;
        if (isdigit(v[i][0]) || (v[i][0] == '-' && isdigit(v[i][1])))
            for (int j = 0; v[i][j]; j++)
            {
                if (v[i][j] == '.')
                    cnt++;
            }
        if (cnt > 1) return 0;
    }
    if (v[i][0] == '(') { k1++; sum++; }
    else if (v[i][0] == ')') { k2++; sum--; }
    if (v[i][0] == ' ')
        return 0;
    if (operator_unar(v[i]) == false && strlen(v[i]) >= 2 && !isdigit(v[i][0]) && v[i][0] != '-')
        return 0;
    if (sum > 0) return 0;

    if (k1 != k2) return 0;

    if (v[i - 1][0] == '(' && v[i][0] == ')')
        return 0;

    if (operator_unar(v[i]) == true && strcmp(v[i + 1], "("))
        return 0;

    if (strchr(")!@#$%&:;""''?{}.,[]|*=></^", v[0][0]) || strchr("(!@#$%&:;""''?.,{}[]|*=></^", v[i][0]))
        return 0;

    cnt = 0;
    if (isdigit(v[i][0]) || (v[i][0] == '-' && isdigit(v[i][1]))) ///Am corectat aici pt cazul cu numere negative cu virgula
        for (int j = 0; v[i][j]; j++)
        {
            if (v[i][j] == '.')
                cnt++;
            if (strchr("()!@#$%&:;""''?{}[],|*=></^", v[i][j]))
                return 0;
        }
    if (cnt > 1) return 0;

    return 1;
}

void arborizare()
{
    int i;
    for(i=0;i<k;i++)
    {
        if(isdigit(q[i][0]) || strcmp("x",q[i])==0 || strcmp("e",q[i])==0 || isdigit(q[i][1]))
        {
            nod *arb=new nod{q[i],nullptr,nullptr};
            operanzi.push(arb);
        }
        else
            if(operator_unar(q[i]))
            {
                if(!operanzi.empty())
                {
                    nod *operand=operanzi.top();
                    operanzi.pop();
                    nod *arb= new nod{q[i],operand,nullptr};
                    operanzi.push(arb);
                }
            }
            else
                if(operanzi.size()>=2)
                {
                    nod *dreapta=operanzi.top();
                    operanzi.pop();
                    nod *stanga=operanzi.top();
                    operanzi.pop();
                    nod *arb=new nod{q[i],stanga,dreapta};
                    operanzi.push(arb);
                }
    }
    if(!operanzi.empty())
    {
        arbore=operanzi.top();
        operanzi.pop();
    }
}

bool paranteze(nod *parinte, nod *fiu, bool asociativitate)
{
   if(!parinte || !fiu)
        return false;

    int prioritate_p=prioritate(parinte->info[0]);
    int prioritate_f=prioritate(fiu->info[0]);

    if(operator_unar(fiu->info))
        return false;

    if(prioritate_f > prioritate_p)
        return false;
    
    if(prioritate_f==prioritate_p)
    {
        if(asociativitate==true)
            return false;
        
        if(parinte->info=="-" || parinte->info=="/")
            return true;
    }
    return true;
    
}

void afisare(nod *radacina, nod* parinte=nullptr, bool asociativitate=true)
{
    if(radacina==NULL)
        return ;
    
    bool necesita_paranteze=false;
    if(!operator_unar(radacina->info))
            necesita_paranteze=paranteze(parinte,radacina,asociativitate);

    if(necesita_paranteze)
        cout<<"(";
    
    if(operator_unar(radacina->info))
    {
        bool ok=false;
        if(radacina->info=="~")
        {
            cout<<"(";
            cout<<"-";
            ok=true;
        }
        else
            cout<<radacina->info;
        if(radacina->st && ok==false)
        {
            cout<<"(";
            ok=true;
        }
        if(radacina->st) 
            afisare(radacina->st,radacina,true);
        else
            if(radacina->dr)
                afisare(radacina->dr,radacina,true);
        if(ok==true)
            cout<<")";
    }
    else
    {
        if(radacina->st && !operator_unar(radacina->info))
            afisare(radacina->st,radacina,true);
        
        cout<<radacina->info;

        if(radacina->dr && !operator_unar(radacina->info))
            afisare(radacina->dr,radacina,false);
    }
    
    if(necesita_paranteze)
        cout<<")";
}

void inordine(nod *radacina)
{
    if(radacina==NULL)
        return ;

    inordine(radacina->st);
    if(radacina->info=="x")
        gasire=true;
    if((operator_unar(radacina->info)==1 && radacina->info[0]!='~') || radacina->info=="/")
        gasire_operator_unar=true;
    if(isdigit(radacina->info[0]))
    {
        int i;
        for(i=0;i<radacina->info.size();i++)
            if(radacina->info[i]=='.')
                gasire_punct=true;
    }
    
    if(radacina->info=="e")
        gasire_e=true;

    inordine(radacina->dr);

}

bool constanta(nod *arb) 
{
    if(isdigit(arb->info[0]) || isdigit(arb->info[1]))
        return true;
    
    if(operator_unar(arb->info) && isdigit(arb->st->info[0]))
        return true;
    if(arb->st!=nullptr && arb->dr!=nullptr)
    {
        if(arb->st->info=="e" && isdigit(arb->dr->info[0]))
            return true;
    }
    if(arb->info=="e") 
        return true;
    return false;
}

void derivare(nod *arb, nod *&arbd)
{
    arbd=new nod;
    if(constanta(arb)==true)
    {
        arbd->info="0";
        arbd->st=nullptr;
        arbd->dr=nullptr;
    }
    else
        if(arb->info=="x")
        {
            arbd->info="1";
            arbd->st=nullptr;
            arbd->dr=nullptr;
        }
        else
            if(arb->info=="+" || arb->info=="-")
            {
                nod *stanga,*dreapta;
                if(arb->st)
                    derivare(arb->st,stanga);
                if(arb->dr)
                    derivare(arb->dr,dreapta);
                arbd->info=arb->info;
                arbd->st=stanga;
                arbd->dr=dreapta;
            }
            else
                if(arb->info=="*")
                {
                    nod *stanga1,*dreapta1, *stanga2,*dreapta2;
                    if(arb->st)derivare(arb->st,stanga1);
                    if(arb->dr)derivare(arb->dr,dreapta1);
                    stanga2= new nod{"*",stanga1,arb->dr};
                    dreapta2= new nod{"*",arb->st,dreapta1};
                    arbd->info="+";
                    arbd->st=stanga2;
                    arbd->dr=dreapta2;
                }
                else
                    if(arb->info=="/" && !isdigit(arb->dr->info[0]))
                    {
                        nod *stanga1, *dreapta1, *stanga2, *dreapta2, *arb2,*stanga3, *dreapta3, *arb3;
                        if(arb->st)derivare(arb->st,stanga1);
                        if(arb->dr)derivare(arb->dr,dreapta1);
                        stanga2=new nod{"*",stanga1,arb->dr};
                        dreapta2=new nod{"*",arb->st,dreapta1};
                        arb2=new nod{"-",stanga2,dreapta2};
                        stanga3=arb->dr;
                        dreapta3=new nod{"2",nullptr,nullptr};
                        arb3=new nod{"^",stanga3,dreapta3};
                        arbd->info="/";
                        arbd->st=arb2;
                        arbd->dr=arb3;
                    }
                    else
                        if(arb->info=="/" && isdigit(arb->dr->info[0]))
                        {
                            nod *stanga;
                            if(arb->st) derivare(arb->st,stanga);
                            arbd->info="/";
                            arbd->st=stanga;
                            arbd->dr=arb->dr;
                            
                        }
                    else
                        if(arb->info=="~")
                        {
                            nod *stanga;
                            if(arb->st)
                                derivare(arb->st,stanga);
                            arbd->info="~";
                            arbd->st=stanga;
                            arbd->dr=nullptr;
                        }
                        else
                            if(arb->info=="sin")
                            {
                                nod *stanga1,*arb1,*arb2;
                                stanga1=new nod{arb->st->info,arb->st->st,arb->st->dr};
                                arb1=new nod{"cos",stanga1,nullptr};
                                if(arb->st)derivare(arb->st,arb2);
                                arbd->info="*";
                                arbd->st=arb1;
                                arbd->dr=arb2;
                            }
                        else
                            if(arb->info=="cos")
                            {
                                nod *stanga1,*dreapta1,*arb1,*arb2;
                                dreapta1=new nod{"sin",arb->st,nullptr};
                                arb1=new nod{"~",dreapta1,nullptr}; // Modificat, si de verificat cred?
                                if(arb->st)derivare(arb->st,arb2);
                                arbd->info="*";
                                arbd->st=arb1;
                                arbd->dr=arb2;
                            }
                            else
                                if(arb->info=="tg")
                                {
                                    nod *stanga1,*dreapta1,*arb1, *stanga2,*dreapta2,*arb2,*arb3;
                                    stanga1=new nod{"cos",arb->st,nullptr};
                                    dreapta1=new nod{"2",nullptr,nullptr};
                                    arb1=new nod{"^",stanga1,dreapta1};

                                    stanga2=new nod{"1",nullptr,nullptr};
                                    dreapta2=arb1;
                                    arb2=new nod{"/",stanga2,dreapta2};
                                    
                                    if(arb->st)derivare(arb->st,arb3);

                                    arbd->info="*";
                                    arbd->st=arb2;
                                    arbd->dr=arb3;
                                }
                                else
                                    if(arb->info=="ctg")
                                    {
                                        nod *stanga1,*dreapta1,*arb1, *stanga2,*dreapta2,*arb2,*arb3, *arb5;
                                        stanga1=new nod{"sin",arb->st,nullptr};
                                        dreapta1=new nod{"2",nullptr,nullptr};
                                        arb1=new nod{"^",stanga1,dreapta1};

                                        stanga2=new nod{"1",nullptr,nullptr};
                                        arb5=new nod{"~",stanga2,nullptr};

                                        dreapta2=arb1;
                                        arb2=new nod{"/",arb5,dreapta2};
                                        
                                        if(arb->st)derivare(arb->st,arb3);

                                        arbd->info="*";
                                        arbd->st=arb2;
                                        arbd->dr=arb3;
                                    }
                                    else
                                        if(arb->info=="arcsin")
                                        {
                                            nod *dreapta1,*arb1,*stanga2,*arb2,*arb3,*stanga4,*arb4,*arb5;

                                            dreapta1=new nod{"2",nullptr,nullptr};
                                            arb1=new nod{"^",arb->st,dreapta1};

                                            stanga2=new nod{"1",nullptr,nullptr};
                                            arb2=new nod{"-",stanga2,arb1};

                                            arb3=new nod{"sqrt",arb2,nullptr};

                                            stanga4=new nod{"1",nullptr,nullptr};
                                            arb4=new nod{"/",stanga4,arb3};

                                            if(arb->st)derivare(arb->st,arb5);

                                            arbd->info="*";
                                            arbd->st=arb4;
                                            arbd->dr=arb5;
                                        }
                                        else
                                            if(arb->info=="arccos") // De verificat aici
                                            {
                                                nod *dreapta1,*arb1,*stanga2,*arb2,*arb3,*stanga4,*arb4,*arb5,*arb6;

                                            dreapta1=new nod{"2",nullptr,nullptr};
                                            arb1=new nod{"^",arb->st,dreapta1};

                                            stanga2=new nod{"1",nullptr,nullptr};
                                            arb2=new nod{"-",stanga2,arb1};

                                            arb3=new nod{"sqrt",arb2,nullptr};


                                            stanga4=new nod{"1",nullptr,nullptr};
                                            arb6=new nod{"~",stanga4,nullptr};
                                            arb4=new nod{"/",arb6,arb3};

                                            if(arb->st)derivare(arb->st,arb5);

                                            arbd->info="*";
                                            arbd->st=arb4;
                                            arbd->dr=arb5;
                                            }
    else
        if(arb->info=="arctg")
        {
            nod *dreapta1,*arb1,*dreapta2,*arb2,*stanga3,*arb3,*arb4;
            
            dreapta1=new nod{"2",nullptr,nullptr};
            arb1=new nod{"^",arb->st,dreapta1};

            dreapta2=new nod{"1",nullptr,nullptr};
            arb2=new nod{"+",arb1,dreapta2};

            stanga3=new nod{"1",nullptr,nullptr};
            arb3=new nod{"/",stanga3,arb2};

            if(arb->st)derivare(arb->st,arb4);
            
            arbd->info="*";
            arbd->st=arb3;
            arbd->dr=arb4;
        }
        else
            if(arb->info=="arcctg")
            {
                nod *dreapta1,*arb1,*dreapta2,*arb2,*stanga3,*arb3,*arb4,*arb5;
            
                dreapta1=new nod{"2",nullptr,nullptr};
                arb1=new nod{"^",arb->st,dreapta1};

                dreapta2=new nod{"1",nullptr,nullptr};
                arb2=new nod{"+",arb1,dreapta2};

                stanga3=new nod{"1",nullptr,nullptr};
                arb3=new nod{"~",stanga3,nullptr};

                arb4=new nod{"/",arb3,arb2};

                if(arb->st)derivare(arb->st,arb5);
            
                arbd->info="*";
                arbd->st=arb4;
                arbd->dr=arb5;
            }
            else
                if(arb->info=="sqrt")
                {
                    nod *arb1,*stanga2,*arb2,*stanga3,*arb3,*arb4;
                    
                    arb1=new nod{"sqrt",arb->st,nullptr};

                    stanga2=new nod{"2",nullptr,nullptr};
                    arb2=new nod{"*",stanga2,arb1};

                    stanga3=new nod{"1",nullptr,nullptr};
                    arb3=new nod{"/",stanga3,arb2};

                    if(arb->st)derivare(arb->st,arb4);

                    arbd->info="*";
                    arbd->st=arb3;
                    arbd->dr=arb4;
                }
                else
                    if(arb->info=="ln")
                    {
                        nod *stanga1,*arb1,*arb2;

                        stanga1=new nod{"1",nullptr,nullptr};
                        arb1=new nod{"/",stanga1,arb->st};

                        if(arb->st)derivare(arb->st,arb2);

                        arbd->info="*";
                        arbd->st=arb1;
                        arbd->dr=arb2;
                    }
                    else
                        if(arb->info=="log")
                        {
                            nod *stanga1,*arb1, *arb2,*stanga3,*arb3,*arb4;

                            stanga1=new nod{"2",nullptr,nullptr};
                            arb1=new nod{"ln",stanga1,nullptr};

                            arb2=new nod{"*",arb->st,arb1};

                            stanga3=new nod{"1",nullptr,nullptr};
                            arb3=new nod{"/",stanga3,arb2};

                            if(arb->st)derivare(arb->st,arb4);

                            arbd->info="*";
                            arbd->st=arb3;
                            arbd->dr=arb4;
                        }
    else
    if(arb->info=="^")
    {
        inordine(arb->st);
        if(gasire==false)
        {
            nod *arb1,*arb2,*arb3,*arb4;

            arb1=new nod{"^",arb->st,arb->dr};
            arb2=new nod{"ln",arb->st,nullptr};
            if(arb->st->info=="e")
                arb2=new nod{"1",nullptr,nullptr};
            arb3=new nod{"*",arb1,arb2};

            if(arb->dr)
                derivare(arb->dr,arb4);
                                
            arbd->info="*";
            arbd->st=arb3;
            arbd->dr=arb4;
        }
        else
        {
            gasire=false;
            inordine(arb->dr);
            if(gasire==true)
            {
                nod *arb1,*arb2,*arb3,*stanga3,*arb4,*dreapta4,*arb5;

                arb1=new nod{"^",arb->st,arb->dr};
                arb2=new nod{"ln",arb->st,nullptr};

                arb3=new nod{"*",arb->dr,arb2};
                derivare(arb3,arb4);
                arbd->info="*";
                arbd->st=arb1;
                arbd->dr=arb4;
            }
            else
            {
                nod *stanga1,*dreapta1,*arb1,*stanga2,*dreapta2,*arb2;
                int inr;
                double dnr;
                gasire_operator_unar=false;
                gasire_e=false;
                string p;
                inordine(arb->dr);

                stanga1=arb->dr;
                if(arb->st)derivare(arb->st,dreapta1);
                arb1=new nod{"*",stanga1,dreapta1};

                if(gasire_operator_unar==true ||gasire_e==true)
                {
                    nod *arb3,*dreapta3;
                    
                    dreapta3=new nod{"1",nullptr,nullptr};
                    arb3=new nod{"-",arb->dr,dreapta3};

                    arb2=new nod{"^",arb->st,arb3};
                }
                else
                {
                    if(gasire_punct==true)
                    {
                      if(arb->dr->info[0]=='~')
                            dnr=stod(arb->dr->st->info)*(-1)+1;
                        else
                            dnr=stod(arb->dr->info)-1;
                        ostringstream oss;
                        oss<<setprecision(10)<<dnr;
                        p=oss.str(); 
                    }
                    else
                    {if(arb->dr->info[0]=='~')
                            inr=stoi(arb->dr->st->info)*(-1)+1;
                        else
                            inr=stoi(arb->dr->info)-1;
                        p=to_string(inr);
                    }

                    dreapta2=new nod{p,nullptr,nullptr};
                    arb2=new nod{"^",arb->st,dreapta2};
                }
                
                arbd->info="*";
                arbd->st=arb1;
                arbd->dr=arb2;
            }
        }
    }
    else
        if(arb->info=="lg")
        {
            nod *stanga1,*arb1, *arb2,*stanga3,*arb3,*arb4;

            stanga1=new nod{"10",nullptr,nullptr};
            arb1=new nod{"ln",stanga1,nullptr};

            arb2=new nod{"*",arb->st,arb1};

            stanga3=new nod{"1",nullptr,nullptr};
            arb3=new nod{"/",stanga3,arb2};

            if(arb->st)derivare(arb->st,arb4);

            arbd->info="*";
            arbd->st=arb3;
            arbd->dr=arb4;
        }
}

void verificare_egale(nod *arb1,nod *arb2, bool &egale)
{
    if(!arb1 || !arb2)
        return ;
    
    if(arb1->info!=arb2->info)
        egale=false;
    
    verificare_egale(arb1->st,arb2->st,egale);
    verificare_egale(arb1->dr,arb2->dr,egale);

}

void simplifica(nod *&radacina)
{
    if(radacina==nullptr)
        return ;
    
    simplifica(radacina->st);
    simplifica(radacina->dr);

    if (radacina->info == "*") 
    {
        if (radacina->st)
            if(radacina->st->info == "1") 
                radacina = radacina->dr; 
        else 
            if (radacina->dr)
                if (radacina->dr->info == "1") 
                    radacina = radacina->st;
        else 
        if ((radacina->st && radacina->st->info == "0") || (radacina->dr && radacina->dr->info == "0"))
        {
            radacina->info = "0";
            radacina->st = radacina->dr = nullptr;
        }
        else
            {
                if(radacina->st && radacina->st->info=="~" && (radacina->st->st && radacina->st->st->info=="1" || radacina->st->dr && radacina->st->dr->info=="1"))
                    {
                        radacina->info="~";
                        radacina->st=nullptr;
                    }
                else
                    if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && radacina->dr->st->info=="1" || radacina->dr->dr && radacina->dr->dr->info=="1"))
                        {
                            radacina->info="~";
                            radacina->dr=nullptr;
                        }

            }

                if(radacina->st && radacina->st->info=="/" && radacina->st->st && radacina->st->st->info=="1")
                {
                        nod *aux=radacina->dr;
                        radacina=radacina->st;
                        radacina->st=aux;
                }
                else
                    if(radacina->dr && radacina->dr->info=="/" && radacina->dr->st && radacina->dr->st->info=="1")
                    {
                        nod *aux=radacina->st;
                        radacina=radacina->dr;
                        radacina->st=aux;
                    }
                    else
                if(radacina->dr && radacina->dr->info=="/")
                {
                    bool egale=true;
                    verificare_egale(radacina->st,radacina->dr->dr,egale);
                    if(egale==true)
                    {
                        radacina=radacina->dr->st;
                    }
                }
                    else
                        if(radacina->st && radacina->st->info=="/")
                        {
                            bool egale=true;
                            verificare_egale(radacina->dr,radacina->st->dr,egale);
                            if(egale==true)
                            {
                                radacina=radacina->st->st;
                            }
                        }
    } 
    else 
        if (radacina->info == "+" || radacina->info == "-") 
        {
            if (radacina->st && radacina->st->info == "0")
                    if(radacina->info=="-" && radacina->dr && radacina->dr->info!="0")
                    {
                        radacina->info="~";
                        radacina->st=nullptr;
                    }
                    else 
                        radacina = radacina->dr;
            else 
                if (radacina->dr && radacina->dr->info == "0") 
                    radacina = radacina->st; 
        }
        else
            if(radacina->info=="^" && radacina->dr && radacina->dr->info=="1")
                radacina=radacina->st;
            else
                if(radacina->info=="^" && radacina->dr && radacina->dr->info=="0")
                    radacina=new nod{"1",nullptr,nullptr};
                else
                    if(radacina->info=="^" && radacina->st && radacina->st->info=="0")
                        radacina=new nod{"0",nullptr,nullptr};
                    else
                        if(radacina->info=="^" && radacina->st && radacina->st->info=="1")
                            radacina=new nod{"1",nullptr,nullptr};
            else
                if(radacina->info=="/")
                {   
                    if(radacina->dr && radacina->dr->info=="1")
                            radacina=radacina->st;
                    else
                        if(radacina->dr && radacina->dr->info=="0")
                        {
                            cout<<"Impartirea la 0 este imposibila";
                            eroare=true;
                        }
                        else
                            {
                                bool egale=true;
                                verificare_egale(radacina->st,radacina->dr,egale);
                                if(egale==true)
                                    radacina=new nod{"1",nullptr,nullptr};
                            }
                }
                else
                    if(radacina->info=="ln" && radacina->st && radacina->st->info=="e" )
                    {
                        radacina->info="1";
                        radacina->st=nullptr;
                    }
                    else
                        if(radacina->info=="ln" && radacina->st && radacina->st->info=="1")
                        {
                            radacina->info="0";
                            radacina->st=nullptr;
                        }
}

int cmmdc(int a,int b)
{
    int r;
    while(b)
    {
        r=a%b;
        a=b;
        b=r;
    }
    return a;
}

void calcule(nod *&radacina) 
{
    if(radacina==nullptr)
        return ;
    
    calcule(radacina->st);
    calcule(radacina->dr);

    if (radacina == nullptr || radacina->st == nullptr ||radacina->dr == nullptr ||
    (!isdigit(radacina->st->info[0]) && 
     (radacina->st->info != "~" || 
      radacina->st->st == nullptr || 
      !isdigit(radacina->st->st->info[0]))) ||
    (!isdigit(radacina->dr->info[0]) && 
     (radacina->dr->info != "~" || 
      radacina->dr->st == nullptr || 
      !isdigit(radacina->dr->st->info[0]))))
    {
        if(radacina->info=="+")
        {
            int is;
            double ds;
            string p;
            bool stg,drt;
            stg=drt=false;

            if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="+" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
            {
                gasire_punct=false;
                inordine(radacina->dr);
                if(gasire_punct==true)
                    drt=true;
                
                gasire_punct=false;
                if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                {
                    inordine(radacina->st->st);
                    if(gasire_punct==true)
                        stg=true;

                    if(stg==true)
                        ds=stod(radacina->st->st->info);
                    else
                        is=stoi(radacina->st->st->info);

                    if((drt==true || drt==false) && stg==true)
                    {
                        ds=ds+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    }

                    if(drt==true && stg==false)
                    {
                        ds=is+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    } 

                    if(drt==false && stg==false)
                    {
                        is=is+stoi(radacina->dr->info);
                        p=to_string(is);
                    }

                    radacina->st->info=p;
                    radacina->dr=radacina->st->dr;
                    radacina->st->st=radacina->st->dr=nullptr;

                }
                else
                if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                {
                    inordine(radacina->st->dr);
                    if(gasire_punct==true)
                        stg=true;

                    if(stg==true)
                        ds=stod(radacina->st->dr->info);
                    else
                        is=stoi(radacina->st->dr->info);

                    if((drt==true || drt==false) && stg==true)
                    {
                        ds=ds+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    }

                    if(drt==true && stg==false)
                    {
                        ds=is+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    } 

                    if(drt==false && stg==false)
                    {
                        is=is+stoi(radacina->dr->info);
                        p=to_string(is);
                    }

                    radacina->st=radacina->st->st;
                    radacina->dr->info=p;
                    radacina->dr->st=radacina->dr->dr=nullptr;
                }
            }
            else
            if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="-" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
            {
                gasire_punct=false;
                inordine(radacina->dr);
                if(gasire_punct==true)
                    drt=true;
                
                gasire_punct=false;
                if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                {
                    inordine(radacina->st->st);
                    if(gasire_punct==true)
                        stg=true;

                    if(stg==true)
                        ds=stod(radacina->st->st->info);
                    else
                        is=stoi(radacina->st->st->info);

                    if((drt==true || drt==false) && stg==true)
                    {
                        ds=ds+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    }

                    if(drt==true && stg==false)
                    {
                        ds=is+stod(radacina->dr->info);
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    } 

                    if(drt==false && stg==false)
                    {
                        is=is+stoi(radacina->dr->info);
                        p=to_string(is);
                    }

                    radacina->st->info=p;
                    radacina->dr=radacina->st->dr;
                    radacina->st->st=radacina->st->dr=nullptr;

                }
                else
                if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                {
                    inordine(radacina->st->dr);
                    if(gasire_punct==true)
                        stg=true;

                    if(stg==true)
                        ds=stod(radacina->st->dr->info)*(-1);
                    else
                        is=stoi(radacina->st->dr->info)*(-1);
                    
                    bool vezi=false;

                    if((drt==true || drt==false) && stg==true)
                    {
                        ds=ds+stod(radacina->dr->info);
                        if(ds<0)
                        {
                            vezi=true;
                            ds=ds*(-1);
                        }
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    }

                    if(drt==true && stg==false)
                    {
                        ds=is+stod(radacina->dr->info);
                        if(ds<0)
                        {
                            vezi=true;
                            ds=ds*(-1);
                        }
                        ostringstream oss;
                        oss<<setprecision(10)<<ds;
                        p=oss.str();
                    } 

                    if(drt==false && stg==false)
                    {
                        is=is+stoi(radacina->dr->info);
                        if(is<0)
                        {
                            vezi=true;
                            is=is*(-1);
                        }
                        p=to_string(is);
                    }

                    if(vezi==true)
                        radacina->info="-";
                    radacina->st=radacina->st->st;
                    radacina->dr->info=p;
                    radacina->dr->st=radacina->dr->dr=nullptr;

                }
            }
            else
                if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="+" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                {
                    gasire_punct=false;
                    inordine(radacina->st);
                    if(gasire_punct==true)
                        stg=true;
                    
                    gasire_punct=false;
                    if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                    {
                        inordine(radacina->dr->st);
                        if(gasire_punct==true)
                            drt=true;

                        if(stg==true)
                            ds=stod(radacina->st->info);
                        else
                            is=stoi(radacina->st->info);

                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds+stod(radacina->dr->st->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is+stod(radacina->dr->st->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is+stoi(radacina->dr->st->info);
                            p=to_string(is);
                        }

                        radacina->st=radacina->dr->dr;
                        radacina->dr->info=p;
                        radacina->dr->st=radacina->dr->dr=nullptr;
                    }
                    else
                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                    {
                        inordine(radacina->dr->dr);
                        if(gasire_punct==true)
                            drt=true;

                        if(stg==true)
                            ds=stod(radacina->st->info);
                        else
                            is=stoi(radacina->st->info);

                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds+stod(radacina->dr->dr->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is+stod(radacina->dr->dr->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is+stoi(radacina->dr->dr->info);
                            p=to_string(is);
                        }

                        radacina->st=radacina->dr->st;
                        radacina->dr->info=p;
                        radacina->dr->st=radacina->dr->dr=nullptr;
                    }
               }
               else
                    if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="-" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                {
                    gasire_punct=false;
                    inordine(radacina->st);
                    if(gasire_punct==true)
                        stg=true;
                    
                    gasire_punct=false;
                    if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                    {
                        inordine(radacina->dr->st);
                        if(gasire_punct==true)
                            drt=true;

                        if(stg==true)
                            ds=stod(radacina->st->info);
                        else
                            is=stoi(radacina->st->info);

                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds+stod(radacina->dr->st->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is+stod(radacina->dr->st->info);
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is+stoi(radacina->dr->st->info);
                            p=to_string(is);
                        }

                        radacina->info="-";
                        radacina->st->info=p;
                        radacina->dr=radacina->dr->dr;
                        radacina->st->st=radacina->st->dr=nullptr;
                    }
                    else
                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                    {
                        inordine(radacina->dr->dr);
                        if(gasire_punct==true)
                            drt=true;

                        if(stg==true)
                            ds=stod(radacina->st->info);
                        else
                            is=stoi(radacina->st->info);

                        bool vezi=false;
                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds+stod(radacina->dr->dr->info)*(-1);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is+stod(radacina->dr->dr->info);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is+stoi(radacina->dr->dr->info);
                            if(is<0)
                            {
                                vezi=true;
                                is=is*(-1);
                            }
                            p=to_string(is);
                        }

                        if(vezi==true)
                            radacina->info="-";
                        radacina->st=radacina->dr->st;
                        radacina->dr->info=p;
                        radacina->dr->st=radacina->dr->dr=nullptr;
                    }
               }
               else
                    if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0])))
                    {
                        radacina->info="-";
                        if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                            radacina->dr=radacina->dr->st;
                        else
                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                radacina->dr=radacina->dr->dr;
                        calcule(radacina);
                    }
                    else
                        if(radacina->st && radacina->st->info=="~" && (radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0])))
                        {
                            radacina->info="-";

                            nod *aux;
                            aux=radacina->dr;
                            if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                radacina->st=radacina->st->st;
                            else
                                if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                    radacina->st=radacina->st->dr;
                            
                            radacina->dr=radacina->st;
                            radacina->st=aux;
                            calcule(radacina);
                        }
                        else
                            if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0])))
                            {
                                if(radacina->st->info=="+")
                                    radacina->st->info="-";
                                else
                                    if(radacina->st->info=="-")
                                        radacina->st->info="+";
                                
                                nod *aux;
                                aux=radacina->st->dr;
                                
                                if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                    radacina->st->dr=radacina->st->st->st;
                                else
                                    if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                        radacina->st->dr=radacina->st->st->dr;
                                radacina->st->st=aux;
                                calcule(radacina);
                            }
                            else
                                if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])))
                                {
                                    if(radacina->dr->info=="+")
                                        radacina->dr->info="-";
                                    else
                                        if(radacina->dr->info=="-")
                                            radacina->dr->info="+";
                                    
                                    nod *aux;
                                    aux=radacina->dr->dr;
                                    
                                    if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                        radacina->dr->dr=radacina->dr->st->st;  
                                    else
                                        if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                            radacina->dr->dr=radacina->dr->st->dr;
                                    radacina->dr->st=aux;
                                    calcule(radacina);
                                }
                                else
                                    if(radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        if(radacina->st->info=="+")
                                            radacina->st->info="-";
                                        else
                                            if(radacina->st->info=="-")
                                                radacina->st->info="+";
                                        
                                        if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                            radacina->st->dr=radacina->st->dr->st;
                                        else
                                            if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                radacina->st->dr=radacina->st->dr->dr;
                                        calcule(radacina);
                                    }
                                        else
                                            if(radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                            {
                                                if(radacina->dr->info=="+")
                                                    radacina->dr->info="-";
                                                else
                                                    if(radacina->dr->info=="-")
                                                        radacina->dr->info="+";
                                                
                                                if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->st;
                                                else
                                                    if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                        radacina->dr->dr=radacina->dr->dr->dr;
                                                calcule(radacina);
                                            }
        }
        else
            if(radacina->info=="-")
            {
                int is;
                double ds;
                string p;
                bool stg,drt;
                stg=drt=false;

                if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="+" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                {
                    
                    gasire_punct=false;
                    inordine(radacina->dr);
                    if(gasire_punct==true)
                        drt=true;
                    
                    gasire_punct=false;
                    if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                    {
                        inordine(radacina->st->st);
                        if(gasire_punct==true)
                            stg=true;

                        if(stg==true)
                            ds=stod(radacina->st->st->info);
                        else
                            is=stoi(radacina->st->st->info);

                        bool vezi=false;
                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds-stod(radacina->dr->info);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is-stod(radacina->dr->info);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is-stoi(radacina->dr->info);
                            if(is<0)
                            {
                                vezi=true;
                                is=is*(-1);
                            }
                            p=to_string(is);
                        }

                        if(vezi==false)
                            radacina->info="+";
                        radacina->st=radacina->st->dr;
                        radacina->dr->info=p;
                        radacina->dr->st=radacina->dr->dr=nullptr;

                    }
                    else
                    if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                    {
                        inordine(radacina->st->dr);
                        if(gasire_punct==true)
                            stg=true;

                        if(stg==true)
                            ds=stod(radacina->st->dr->info);
                        else
                            is=stoi(radacina->st->dr->info);

                        bool vezi=false;
                        if((drt==true || drt==false) && stg==true)
                        {
                            ds=ds-stod(radacina->dr->info);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }

                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        }

                        if(drt==true && stg==false)
                        {
                            ds=is-stod(radacina->dr->info);
                            if(ds<0)
                            {
                                vezi=true;
                                ds=ds*(-1);
                            }
                            ostringstream oss;
                            oss<<setprecision(10)<<ds;
                            p=oss.str();
                        } 

                        if(drt==false && stg==false)
                        {
                            is=is-stoi(radacina->dr->info);
                            if(is<0)
                            {
                                vezi=true;
                                is=is*(-1);
                            }
                            p=to_string(is);
                        }

                        if(vezi==false)
                            radacina->info="+";
                        radacina->st=radacina->st->st;
                        radacina->dr->info=p;
                        radacina->dr->st=radacina->dr->dr=nullptr;
                    }
                }
                else
                    if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="+" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                    {
                        gasire_punct=false;
                        inordine(radacina->st);
                        if(gasire_punct==true)
                            stg=true;
                        
                        gasire_punct=false;
                        if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                        {
                            inordine(radacina->dr->st);
                            if(gasire_punct==true)
                                drt=true;

                            if(stg==true)
                                ds=stod(radacina->st->info);
                            else
                                is=stoi(radacina->st->info);

                            bool vezi=false;
                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds-stod(radacina->dr->st->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is-stod(radacina->dr->st->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is-stoi(radacina->dr->st->info);
                                if(is<0)
                                {
                                    vezi=true;
                                    is=is*(-1);
                                }
                                p=to_string(is);
                            }

                            if(vezi==true)
                            {
                                nod *arb1;
                                arb1=new nod{p,nullptr,nullptr};
                                radacina->st->info="~";
                                radacina->st->st=arb1;
                                radacina->dr=radacina->dr->dr;
                            }
                            else
                            {
                                radacina->st->info=p;
                                radacina->dr=radacina->dr->dr;
                                radacina->st->st=radacina->st->dr=nullptr;
                            }
                            
                        }
                        else
                        if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                        {
                            inordine(radacina->dr->dr);
                            if(gasire_punct==true)
                                drt=true;

                            if(stg==true)
                                ds=stod(radacina->st->info);
                            else
                                is=stoi(radacina->st->info);

                            bool vezi=false;
                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds-stod(radacina->dr->dr->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is-stod(radacina->dr->dr->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is-stoi(radacina->dr->dr->info);
                                if(is<0)
                                {
                                    vezi=true;
                                    is=is*(-1);
                                }
                                p=to_string(is);
                            }

                            if(vezi==true)
                            {
                                nod *arb1;
                                arb1=new nod{p,nullptr,nullptr};
                                radacina->st->info="~";
                                radacina->st->st=arb1;
                                radacina->dr=radacina->dr->st;
                            }
                            else
                            {
                                radacina->st->info=p;
                                radacina->dr=radacina->dr->st;
                                radacina->st->st=radacina->st->dr=nullptr;
                            }
                        }
                }
                else
                    if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="-" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                    {
                        gasire_punct=false;
                        inordine(radacina->dr);
                        if(gasire_punct==true)
                            drt=true;
                        
                        gasire_punct=false;
                        if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                        {
                            inordine(radacina->st->st);
                            if(gasire_punct==true)
                                stg=true;

                            if(stg==true)
                                ds=stod(radacina->st->st->info);
                            else
                                is=stoi(radacina->st->st->info);

                            bool vezi=false;
                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds-stod(radacina->dr->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is-stod(radacina->dr->info);
                                if(ds<0)
                                {
                                    vezi=true;
                                    ds=ds*(-1);
                                }
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is-stoi(radacina->dr->info);
                                if(is<0)
                                {
                                    vezi=true;
                                    is=is*(-1);
                                }
                                p=to_string(is);
                            }

                            nod *aux;
                            aux=radacina->dr;
                            radacina->dr=radacina->st;
                            radacina->st=aux;

                            if(vezi==true)
                            {

                                nod *arb1;
                                arb1=new nod{p,nullptr,nullptr};
                                radacina->st=new nod{"~",arb1,nullptr};
                                radacina->dr=radacina->dr->dr;
                            }
                            else
                            {
                                radacina->st=new nod{p,nullptr,nullptr};
                                radacina->dr=radacina->dr->dr;
                            }
                        }
                        else
                        if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                        {
                            inordine(radacina->st->dr);
                            if(gasire_punct==true)
                                stg=true;

                            if(stg==true)
                                ds=stod(radacina->st->dr->info);
                            else
                                is=stoi(radacina->st->dr->info);

                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds*(-1)-stod(radacina->dr->info);
                                ds=ds*(-1);
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is*(-1)-stod(radacina->dr->info);
                                ds=ds*(-1);
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is*(-1)-stoi(radacina->dr->info);
                                is=is*(-1);
                                p=to_string(is);
                            }

                            radacina->st=radacina->st->st;
                            radacina->dr->info=p;
                            radacina->dr->st=radacina->dr->dr=nullptr;
                        }
                    }
                    else
                        if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="-" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                        {
                            gasire_punct=false;
                            inordine(radacina->st);
                            if(gasire_punct==true)
                                stg=true;
                            
                            gasire_punct=false;
                            if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                            {
                                inordine(radacina->dr->st);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                bool vezi=false;
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds-stod(radacina->dr->st->info);
                                    if(ds<0)
                                    {
                                        vezi=true;
                                        ds=ds*(-1);
                                    }
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is-stod(radacina->dr->st->info);
                                    if(ds<0)
                                    {
                                        vezi=true;
                                        ds=ds*(-1);
                                    }
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is-stoi(radacina->dr->st->info);
                                    if(is<0)
                                    {
                                        vezi=true;
                                        is=is*(-1);
                                    }
                                    p=to_string(is);
                                }

                                if(vezi==false)
                                    radacina->info="+";
                                
                                nod *aux;
                                aux=radacina->st;
                                radacina->st=radacina->dr->dr;
                                radacina->dr=aux;
                                radacina->dr->info=p;
                                radacina->dr->st=radacina->dr->dr=nullptr;
                                
                            }
                            else
                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                            {
                                inordine(radacina->dr->dr);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds-stod(radacina->dr->dr->info)*(-1);
    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is-stod(radacina->dr->dr->info)*(-1);
                                   
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is-stoi(radacina->dr->dr->info)*(-1);
                                   
                                    p=to_string(is);
                                }

                                radacina->st->info=p;
                                radacina->dr=radacina->dr->st;
                                radacina->st->st=radacina->st->dr=nullptr;
                            }
                    }
                else
                    if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0])))
                    {
                        radacina->info="+";
                        if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                            radacina->dr=radacina->dr->st;
                        else
                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                radacina->dr=radacina->dr->dr;
                        calcule(radacina);
                    }
                    else
                        if(radacina->st && radacina->st->info=="~" && (radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0])))
                        {
                            if(radacina->dr->info=="+")
                            {
                                radacina->dr->info="-";

                                nod *arb1;
                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                    arb1=new nod{"~",radacina->dr->dr,nullptr};
                                else
                                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                        arb1=new nod{"~",radacina->dr->st,nullptr};
                                
                                nod *aux;
                                if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                    aux=radacina->st->st;
                                else
                                    if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                        aux=radacina->st->dr;

                                radacina->st=radacina->dr;
                                radacina->dr=aux;

                                radacina->st->st=arb1;
                                calcule(radacina);
                            }
                            else
                                if(radacina->dr->info=="-")
                                {
                                    nod *aux;

                                    aux=radacina->dr->dr;
                                    radacina->dr->dr=radacina->dr->st;
                                    radacina->dr->st=aux;

                                    if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                        aux=radacina->st->st;
                                    else
                                        if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                            aux=radacina->st->dr;
                                    radacina->st=radacina->dr;
                                    radacina->dr=aux;
                                    calcule(radacina);
                                }
                        }
                        else
                            if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0])))
                            {
                                if(radacina->st->info=="+")
                                {
                                    radacina->st->info="-";

                                    nod *aux;
                                    aux=radacina->st->dr;
                                    if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                    {
                                        radacina->st->dr=radacina->st->st->st;
                                        radacina->st->st->st=aux;
                                    }
                                    else
                                        if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                        {
                                            radacina->st->dr=radacina->st->st->dr;
                                            radacina->st->st->dr=aux;
                                        }
                                }
                                else
                                    if(radacina->st->info=="-")
                                    {
                                        nod *aux;
                                        aux=radacina->st->dr;
                                        if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                        {
                                            radacina->st->dr=radacina->st->st->st;
                                            radacina->st->st->st=aux;
                                        }
                                        else
                                            if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                            {
                                                radacina->st->dr=radacina->st->st->dr;
                                                radacina->st->st->dr=aux;
                                            }
                                    }
                                    
                                calcule(radacina);
                            }
                            else
                                if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])))
                                {
                                    if(radacina->dr->info=="+")
                                    {
                                        radacina->dr->info="-";
                                        radacina->info="+";
                                    }
                                    else
                                        if(radacina->dr->info=="-")
                                        {
                                            radacina->dr->info="+";
                                            radacina->info="+";
                                        }

                                     if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                            radacina->dr->st=radacina->dr->st->st;
                                    else
                                        if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                            radacina->dr->st=radacina->dr->st->dr;

                                    calcule(radacina);
                                }
                                else
                                    if(radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        if(radacina->st->info=="+")
                                        {
                                            radacina->st->info="-";
                                            if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                                radacina->st->dr=radacina->st->dr->st;
                                            else
                                                if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                    radacina->st->dr=radacina->st->dr->dr;
                                        }
                                        else
                                            if(radacina->st->info=="-")
                                            {
                                                radacina->st->info="+";
                                                if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                                    radacina->st->dr=radacina->st->dr->st;
                                                else
                                                    if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                        radacina->st->dr=radacina->st->dr->dr;
                                            }
                                        calcule(radacina);
                                    }
                                        else
                                            if(radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                            {
                                                if(radacina->dr->info=="+")
                                                    radacina->dr->info="-";
                                                else
                                                    if(radacina->dr->info=="-")
                                                        radacina->dr->info="+";
                                                
                                                if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->st;
                                                else
                                                    if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                        radacina->dr->dr=radacina->dr->dr->dr;
                                                calcule(radacina);
                                            }
            }
            else
                if(radacina->info=="*")
                {
                    int is,dvz=0;
                    double ds;
                    string p,q;
                    bool stg,drt;
                    stg=drt=false;

 
                    if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="*" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                    {
                        gasire_punct=false;
                        inordine(radacina->dr);
                        if(gasire_punct==true)
                            drt=true;
                        
                        gasire_punct=false;
                        if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                        {
                            inordine(radacina->st->st);
                            if(gasire_punct==true)
                                stg=true;

                            if(stg==true)
                                ds=stod(radacina->st->st->info);
                            else
                                is=stoi(radacina->st->st->info);

                            
                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds*stod(radacina->dr->info);
        
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is*stod(radacina->dr->info);
                                
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is*stoi(radacina->dr->info);
                                
                                
                                p=to_string(is);
                            }

                            radacina->dr=radacina->st->dr;
                            radacina->st->info=p;
                            radacina->st->st=radacina->st->dr=nullptr;

                        }
                        else
                        if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                        {
                            inordine(radacina->st->dr);
                            if(gasire_punct==true)
                                stg=true;

                            if(stg==true)
                                ds=stod(radacina->st->dr->info);
                            else
                                is=stoi(radacina->st->dr->info);

                            if((drt==true || drt==false) && stg==true)
                            {
                                ds=ds*stod(radacina->dr->info);
                               

                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            }

                            if(drt==true && stg==false)
                            {
                                ds=is*stod(radacina->dr->info);
                                
                                ostringstream oss;
                                oss<<setprecision(10)<<ds;
                                p=oss.str();
                            } 

                            if(drt==false && stg==false)
                            {
                                is=is*stoi(radacina->dr->info);
                               
                                p=to_string(is);
                            }

                            radacina->dr=radacina->st->st;
                            radacina->st->info=p;
                            radacina->st->st=radacina->st->dr=nullptr;
                        }
                    }
                    else
                        if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="/" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                        {
                            
                            gasire_punct=false;
                            inordine(radacina->dr);
                            if(gasire_punct==true)
                                drt=true;
                            
                            gasire_punct=false;
                            if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                            {
                                inordine(radacina->st->st);
                                if(gasire_punct==true)
                                    stg=true;

                                if(stg==true)
                                    ds=stod(radacina->st->st->info);
                                else
                                    is=stoi(radacina->st->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds*stod(radacina->dr->info);
            
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is*stod(radacina->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is*stoi(radacina->dr->info);
                                    
                                    
                                    p=to_string(is);
                                }

                                radacina->info="/";
                                radacina->dr=radacina->st->dr;
                                radacina->st->info=p;
                                radacina->st->st=radacina->st->dr=nullptr;

                            }
                            else
                            if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                            {
                                inordine(radacina->st->dr);
                                if(gasire_punct==true)
                                    stg=true;

                                if(stg==true)
                                    ds=stod(radacina->st->dr->info);
                                else
                                    is=stoi(radacina->st->dr->info);

                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds/stod(radacina->dr->info);
                                

                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is/stod(radacina->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    int abc;
                                    abc=stoi(radacina->dr->info);
                                    
                                    if(abc%is==0)
                                        abc=abc/is;
                                    else
                                    {
                                        dvz=cmmdc(is,abc);
                                        is=is/dvz;
                                        abc=abc/dvz;
                                        
                                    }
                                    q=to_string(abc);
                                    p=to_string(is);
                                }

                                if(dvz==0)
                                {
                                    radacina->dr=radacina->st->st;
                                    radacina->st->info=q;
                                    radacina->st->st=radacina->st->dr=nullptr;
                                }
                                else
                                {
                                    nod *arb1,*stanga1,*dreapta1;
                                    stanga1=new nod{q};
                                    dreapta1=new nod{p};
                                    arb1=new nod{"/",stanga1,dreapta1};
                                    radacina->dr=radacina->st->st;
                                    radacina->st=arb1;
                                }
                            }
                        }
                    else
                        if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="*" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                        {
                            gasire_punct=false;
                            inordine(radacina->st);
                            if(gasire_punct==true)
                                stg=true;
                            
                            gasire_punct=false;
                            if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                            {
                                inordine(radacina->dr->st);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds*stod(radacina->dr->st->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is*stod(radacina->dr->st->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is*stoi(radacina->dr->st->info);
                                    
                                    p=to_string(is);
                                }

                                radacina->st->info=p;
                                radacina->dr=radacina->dr->dr;
                                
                            }
                            else
                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                            {
                                inordine(radacina->dr->dr);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds*stod(radacina->dr->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is*stod(radacina->dr->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is*stoi(radacina->dr->dr->info);
                                    
                                    p=to_string(is);
                                }

                                radacina->st->info=p;
                                radacina->dr=radacina->dr->st;
                            }
                    }
                    else
                        if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="/" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                        {
                            gasire_punct=false;
                            inordine(radacina->st);
                            if(gasire_punct==true)
                                stg=true;
                            
                            gasire_punct=false;
                            if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                            {
                                inordine(radacina->dr->st);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds*stod(radacina->dr->st->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is*stod(radacina->dr->st->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is*stoi(radacina->dr->st->info);
                                    
                                    p=to_string(is);
                                }

                                radacina->st->info=p;
                                radacina->info="/";
                                radacina->dr=radacina->dr->dr;
                                
                            }
                            else
                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                            {
                                inordine(radacina->dr->dr);
                                if(gasire_punct==true)
                                    drt=true;

                                if(stg==true)
                                    ds=stod(radacina->st->info);
                                else
                                    is=stoi(radacina->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds/stod(radacina->dr->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is/stod(radacina->dr->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    int abc;
                                    abc=stoi(radacina->dr->dr->info);
                                    
                                    if(is%abc==0)
                                        is=is/abc;
                                    else
                                    {
                                        dvz=cmmdc(is,abc);
                                        is=is/dvz;
                                        abc=abc/dvz;
                                        q=to_string(abc);
                                    }
                                    
                                    p=to_string(is);
                                }

                                if(dvz==0)
                                {
                                    radacina->st->info=p;
                                    radacina->dr=radacina->dr->st;
                                    radacina->st->st=radacina->st->dr=nullptr;
                                }
                                else
                                {
                                    nod *arb1,*stanga1,*dreapta1;
                                    stanga1=new nod{p};
                                    dreapta1=new nod{q};
                                    arb1=new nod{"/",stanga1,dreapta1};
                                    radacina->st=arb1;
                                    radacina->dr=radacina->dr->st;
                                }
                            }
                    }
                    else
                       if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0])))
                        {
                            nod *arb1;

                            if(radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                            {
                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                    radacina->dr=radacina->dr->st;
                                else
                                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                        radacina->dr=radacina->dr->dr;
                                
                                calcule(radacina);
                                arb1=new nod{"~",radacina->st,nullptr};
                                radacina->st=arb1;
                            }
                            else
                                if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                || radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                {
                                    
                                        if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                            radacina->dr=radacina->dr->st;
                                        else
                                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                                radacina->dr=radacina->dr->dr;
    

                                    if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                        radacina->st->st=radacina->st->st->st;
                                    else
                                        if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                            radacina->st->st=radacina->st->st->dr;
                                    
                                    if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                        radacina->st->dr=radacina->st->dr->st;
                                    else
                                        if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                            radacina->st->dr=radacina->st->dr->dr;
                                    calcule(radacina);
                                }
                        }
                        else
                            if(radacina->st && radacina->st->info=="~" && (radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0])))
                            {
                                nod *arb1;

                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                {
                                    if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                    radacina->st=radacina->st->st;
                                    else
                                        if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                            radacina->st=radacina->st->dr;

                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                }
                                else
                                    if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])) 
                                    || radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                    {
                                        
                                            if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                                radacina->st=radacina->st->st;
                                            else
                                                if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                                    radacina->st=radacina->st->dr;
                                        
                                            
                                            if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                                radacina->dr->st=radacina->dr->st->st;
                                            else
                                                if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                                    radacina->dr->st=radacina->dr->st->dr;
                                            
                                            if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                radacina->dr->dr=radacina->dr->dr->st;
                                            else
                                                if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->dr;
                                            
                                            calcule(radacina);
                                    }
                            }
                            else 
                                if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0])))
                                {
                                    nod *arb1;

                                    if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                        radacina->st->st=radacina->st->st->st;
                                    else
                                        if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                            radacina->st->st=radacina->st->st->dr;
                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                    
                                }
                            else
                                if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])))
                                {
                                    nod *arb1;
                                    if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                        radacina->dr->st=radacina->dr->st->st;
                                    else
                                        if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                            radacina->dr->st=radacina->dr->st->dr;
                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                }
                                else
                                    if(radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        nod *arb1;
                                        if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                            radacina->st->dr=radacina->st->dr->st;
                                        else
                                            if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                radacina->st->dr=radacina->st->dr->dr;   
                                        calcule(radacina);
                                        arb1=new nod{"~",radacina->st,nullptr};
                                        radacina->st=arb1;
                                    }
                                        else
                                            if(radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                            {
                                                nod *arb1;
                                                if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->st;
                                                else
                                                    if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                        radacina->dr->dr=radacina->dr->dr->dr;
                                                calcule(radacina);
                                                arb1=new nod{"~",radacina->st,nullptr};
                                                radacina->st=arb1;
                                            }
                            
                }
                else
                    if(radacina->info=="^") 
                    {
                        int is;
                        double ds;
                        string p;
                        bool stg,drt;
                        stg=drt=false;

                        if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="^" && radacina->st->st && radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                        {
                            gasire_punct=false;
                            inordine(radacina->dr);
                            if(gasire_punct==true)
                                drt=true;
                            
                            gasire_punct=false;
                            
                            if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                            {
                                inordine(radacina->st->dr);
                                if(gasire_punct==true)
                                    stg=true;

                                if(stg==true)
                                    ds=stod(radacina->st->dr->info);
                                else
                                    is=stoi(radacina->st->dr->info);

                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds*stod(radacina->dr->info);
                                

                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is*stod(radacina->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    is=is*stoi(radacina->dr->info);
                                
                                    p=to_string(is);
                                }

                                radacina->st=radacina->st->st;
                                radacina->dr->info=p;
                                radacina->dr->st=radacina->dr->dr=nullptr;
                            }
                        }
                        else
                            if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0])))
                            {
                                nod *arb1;

                                if(radacina->st->st && radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                {
                                    if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                        radacina->dr=radacina->dr->st;
                                    else
                                        if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                            radacina->dr=radacina->dr->dr;
                                    
                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->dr,nullptr};
                                    radacina->dr=arb1;
                                }
                                else
                                    if(radacina->st->st && radacina->st->dr && radacina->st->dr->info=="~" && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        
                                            if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                                radacina->dr=radacina->dr->st;
                                            else
                                                if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                                    radacina->dr=radacina->dr->dr;
        
                                        
                                        if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                            radacina->st->dr=radacina->st->dr->st;
                                        else
                                            if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                radacina->st->dr=radacina->st->dr->dr;
                                        calcule(radacina);
                                    }
                            }
                            else
                                if(radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        nod *arb1;
                                        if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                            radacina->st->dr=radacina->st->dr->st;
                                        else
                                            if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                radacina->st->dr=radacina->st->dr->dr;   
                                        calcule(radacina);
                                        arb1=new nod{"~",radacina->dr,nullptr};
                                        radacina->dr=arb1;
                                    }
                    }
                    else
                        if(radacina->info=="/")
                        {
                            int is,dvz=0;
                            double ds;
                            string p,q;
                            bool stg,drt;
                            stg=drt=false;

                        if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="*" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                        {
                            
                            gasire_punct=false;
                            inordine(radacina->dr);
                            if(gasire_punct==true)
                                drt=true;
                            
                            gasire_punct=false;
                            if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                            {
                                inordine(radacina->st->st);
                                if(gasire_punct==true)
                                    stg=true;

                                if(stg==true)
                                    ds=stod(radacina->st->st->info);
                                else
                                    is=stoi(radacina->st->st->info);

                                
                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds/stod(radacina->dr->info);
            
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is/stod(radacina->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    int abc=stoi(radacina->dr->info);
                                    
                                    if(is%abc==0)
                                        is=is/abc;
                                    else
                                    {
                                        dvz=cmmdc(is,abc);
                                        is=is/dvz;
                                        abc=abc/dvz;
                                    }
                                    
                                    q=to_string(abc);
                                    p=to_string(is);
                                }

                                if(dvz==0)
                                {
                                    radacina->info="*";
                                    radacina->st->info=p;
                                    radacina->dr=radacina->st->dr;
                                    radacina->st->st=radacina->st->dr=nullptr;
                                }
                                else
                                {
                                    
                                    radacina->st->st->info=p;
                                    radacina->dr->info=q;
                                }

                            }
                            else
                            if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                            {
                                inordine(radacina->st->dr);
                                if(gasire_punct==true)
                                    stg=true;

                                if(stg==true)
                                    ds=stod(radacina->st->dr->info);
                                else
                                    is=stoi(radacina->st->dr->info);

                                if((drt==true || drt==false) && stg==true)
                                {
                                    ds=ds/stod(radacina->dr->info);
                                

                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                }

                                if(drt==true && stg==false)
                                {
                                    ds=is/stod(radacina->dr->info);
                                    
                                    ostringstream oss;
                                    oss<<setprecision(10)<<ds;
                                    p=oss.str();
                                } 

                                if(drt==false && stg==false)
                                {
                                    int abc;
                                    abc=stoi(radacina->dr->info);
                                    
                                    if(is%abc==0)
                                        is=is/abc;
                                    else
                                    {
                                        dvz=cmmdc(is,abc);
                                        is=is/dvz;
                                        abc=abc/dvz;
                                        
                                    }
                                    q=to_string(abc);
                                    p=to_string(is);
                                }

                                if(dvz==0)
                                {
                                    radacina->info="*";
                                    radacina->dr=radacina->st->st;
                                    radacina->st->info=p;
                                    radacina->st->st=radacina->st->dr=nullptr;
                                }
                                else
                                {
                                    radacina->st->dr->info=p;
                                    radacina->dr->info=q;
                                }
                            }
                        }
                        else
                            if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="*" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                            {
                                gasire_punct=false;
                                inordine(radacina->st);
                                if(gasire_punct==true)
                                    stg=true;
                                
                                gasire_punct=false;
                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                {
                                    inordine(radacina->dr->st);
                                    if(gasire_punct==true)
                                        drt=true;

                                    if(stg==true)
                                        ds=stod(radacina->st->info);
                                    else
                                        is=stoi(radacina->st->info);

                                    
                                    if((drt==true || drt==false) && stg==true)
                                    {
                                        ds=ds/stod(radacina->dr->st->info);
                                        
                                        ostringstream oss;
                                        oss<<setprecision(10)<<ds;
                                        p=oss.str();
                                    }

                                    if(drt==true && stg==false)
                                    {
                                        ds=is/stod(radacina->dr->st->info);
                                        
                                        ostringstream oss;
                                        oss<<setprecision(10)<<ds;
                                        p=oss.str();
                                    } 

                                    if(drt==false && stg==false)
                                    {
                                        int abc=stoi(radacina->dr->st->info);
                                        
                                        if(is%abc==0)
                                            is=is/abc;
                                        else
                                        {
                                            dvz=cmmdc(abc,is);
                                            is=is/dvz;
                                            abc=abc/dvz;
                                        }

                                        q=to_string(abc);
                                        p=to_string(is);
                                    }

                                    if(dvz==0)
                                    {
                                        radacina->st->info=p;
                                        radacina->dr=radacina->dr->dr;
                                        radacina->st->st=radacina->st->dr=nullptr;
                                    }
                                    else
                                    {
                                        radacina->st->info=p;
                                        radacina->dr->st->info=q;
                                    }
                                    
                                }
                                else
                                if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                {
                                    inordine(radacina->dr->dr);
                                    if(gasire_punct==true)
                                        drt=true;

                                    if(stg==true)
                                        ds=stod(radacina->st->info);
                                    else
                                        is=stoi(radacina->st->info);

                                    
                                    if((drt==true || drt==false) && stg==true)
                                    {
                                        ds=ds/stod(radacina->dr->dr->info);
                                        
                                        ostringstream oss;
                                        oss<<setprecision(10)<<ds;
                                        p=oss.str();
                                    }

                                    if(drt==true && stg==false)
                                    {
                                        ds=is/stod(radacina->dr->dr->info);
                                        
                                        ostringstream oss;
                                        oss<<setprecision(10)<<ds;
                                        p=oss.str();
                                    } 

                                    if(drt==false && stg==false)
                                    {
                                        int abc=stoi(radacina->dr->dr->info);

                                        if(is%abc==0)
                                            is=is/abc;
                                        else
                                        {
                                            dvz=cmmdc(is,abc);
                                            is=is/dvz;
                                            abc=abc/dvz;
                                        }
                                        q=to_string(abc);
                                        p=to_string(is);
                                    }

                                    if(dvz==0)
                                    {
                                        radacina->st->info=p;
                                        radacina->dr=radacina->dr->st;
                                       radacina->st->st=radacina->st->dr=nullptr;
                                    }
                                    else
                                    {
                                        radacina->st->info=p;
                                        radacina->dr->dr->info=q;
                                    }
                                }
                    }
                            else
                                if(isdigit(radacina->dr->info[0]) && radacina->st && radacina->st->info=="/" && radacina->st->st && radacina->st->dr &&(isdigit(radacina->st->st->info[0]) || isdigit(radacina->st->dr->info[0])))
                                {
                                    gasire_punct=false;
                                    inordine(radacina->dr);
                                    if(gasire_punct==true)
                                        drt=true;
                                    
                                    gasire_punct=false;
                                    if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                    {
                                        nod *aux;
                                        string auxx;

                                        radacina->st->info="*";
                                        
                                        auxx=radacina->dr->info;
                                        radacina->dr->info=radacina->st->st->info;
                                        radacina->st->st->info=auxx;

                                        aux=radacina->st;
                                        radacina->st=radacina->dr;
                                        radacina->dr=aux;

                                        calcule(radacina);

                                    }
                                    else
                                    if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                    {
                                        inordine(radacina->st->dr);
                                        if(gasire_punct==true)
                                            stg=true;

                                        if(stg==true)
                                            ds=stod(radacina->st->dr->info);
                                        else
                                            is=stoi(radacina->st->dr->info);

                                        if((drt==true || drt==false) && stg==true)
                                        {
                                            ds=ds*stod(radacina->dr->info);
                                        

                                            ostringstream oss;
                                            oss<<setprecision(10)<<ds;
                                            p=oss.str();
                                        }

                                        if(drt==true && stg==false)
                                        {
                                            ds=is*stod(radacina->dr->info);
                                            
                                            ostringstream oss;
                                            oss<<setprecision(10)<<ds;
                                            p=oss.str();
                                        } 

                                        if(drt==false && stg==false)
                                        {
                                            is=is*stoi(radacina->dr->info);
                                        
                                            p=to_string(is);
                                        }

                                        radacina->st=radacina->st->st;
                                        radacina->dr->info=p;
                                        radacina->dr->st=radacina->dr->dr=nullptr;
                                    }
                                }
                            else
                                if(isdigit(radacina->st->info[0]) && radacina->dr && radacina->dr->info=="/" && radacina->dr->st && radacina->dr->dr && (isdigit(radacina->dr->st->info[0]) || isdigit(radacina->dr->dr->info[0])))
                                {
                                    gasire_punct=false;
                                    inordine(radacina->st);
                                    if(gasire_punct==true)
                                        stg=true;
                                    
                                    gasire_punct=false;
                                    if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                    {
                                        nod *aux;
                                        string auxx;

                                        radacina->dr->info="*";

                                        auxx=radacina->st->info;
                                        radacina->st->info=radacina->dr->st->info;
                                        radacina->dr->st->info=auxx;

                                        aux=radacina->st;
                                        radacina->st=radacina->dr;
                                        radacina->dr=aux;

                                        calcule(radacina);

                                    }
                                    else
                                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                    {
                                        inordine(radacina->dr->dr);
                                        if(gasire_punct==true)
                                            drt=true;

                                        if(stg==true)
                                            ds=stod(radacina->st->info);
                                        else
                                            is=stoi(radacina->st->info);

                                        
                                        if((drt==true || drt==false) && stg==true)
                                        {
                                            ds=ds*stod(radacina->dr->dr->info);
                                            
                                            ostringstream oss;
                                            oss<<setprecision(10)<<ds;
                                            p=oss.str();
                                        }

                                        if(drt==true && stg==false)
                                        {
                                            ds=is*stod(radacina->dr->dr->info);
                                            
                                            ostringstream oss;
                                            oss<<setprecision(10)<<ds;
                                            p=oss.str();
                                        } 

                                        if(drt==false && stg==false)
                                        {
                                            is=is*stoi(radacina->dr->dr->info);
                                            
                                            p=to_string(is);
                                        }

                                        radacina->st->info=p;
                                        radacina->dr=radacina->dr->st;
                                        radacina->st->st=radacina->st->dr=nullptr;
                                    }
                            }
                            else
                        if(radacina->dr && radacina->dr->info=="~" && (radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0])))
                        {
                            nod *arb1;

                            if(radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                            {
                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                    radacina->dr=radacina->dr->st;
                                else
                                    if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                        radacina->dr=radacina->dr->dr;
                                
                                calcule(radacina);
                                arb1=new nod{"~",radacina->st,nullptr};
                                radacina->st=arb1;
                            }
                            else
                                if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                || radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                {
                                    
                                        if(radacina->dr->st && isdigit(radacina->dr->st->info[0]))
                                            radacina->dr=radacina->dr->st;
                                        else
                                            if(radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                                radacina->dr=radacina->dr->dr;
    

                                    if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                        radacina->st->st=radacina->st->st->st;
                                    else
                                        if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                            radacina->st->st=radacina->st->st->dr;
                                    
                                    if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                        radacina->st->dr=radacina->st->dr->st;
                                    else
                                        if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                            radacina->st->dr=radacina->st->dr->dr;
                                    calcule(radacina);
                                }
                        }
                        else
                            if(radacina->st && radacina->st->info=="~" && (radacina->st->st && isdigit(radacina->st->st->info[0]) || radacina->st->dr && isdigit(radacina->st->dr->info[0])))
                            {
                                nod *arb1;

                                if(radacina->dr->st && isdigit(radacina->dr->st->info[0]) || radacina->dr->dr && isdigit(radacina->dr->dr->info[0]))
                                {
                                    if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                    radacina->st=radacina->st->st;
                                    else
                                        if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                            radacina->st=radacina->st->dr;

                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                }
                                else
                                    if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])) 
                                    || radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                    {
                                        
                                            if(radacina->st->st && isdigit(radacina->st->st->info[0]))
                                                radacina->st=radacina->st->st;
                                            else
                                                if(radacina->st->dr && isdigit(radacina->st->dr->info[0]))
                                                    radacina->st=radacina->st->dr;
                                        
                                            
                                            if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                                radacina->dr->st=radacina->dr->st->st;
                                            else
                                                if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                                    radacina->dr->st=radacina->dr->st->dr;
                                            
                                            if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                radacina->dr->dr=radacina->dr->dr->st;
                                            else
                                                if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->dr;
                                            
                                            calcule(radacina);
                                    }
                            }
                            else 
                                if(radacina->st->st && radacina->st->st->info=="~" && radacina->st->dr && (radacina->st->st->st && isdigit(radacina->st->st->st->info[0]) || radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0])))
                                {
                                    nod *arb1;

                                    if(radacina->st->st->st && isdigit(radacina->st->st->st->info[0]))
                                        radacina->st->st=radacina->st->st->st;
                                    else
                                        if(radacina->st->st->dr && isdigit(radacina->st->st->dr->info[0]))
                                            radacina->st->st=radacina->st->st->dr;
                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                    
                                }
                            else
                                if(radacina->dr->st && radacina->dr->st->info=="~" && radacina->dr->dr && (radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]) || radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0])))
                                {
                                    nod *arb1;
                                    if(radacina->dr->st->st && isdigit(radacina->dr->st->st->info[0]))
                                        radacina->dr->st=radacina->dr->st->st;
                                    else
                                        if(radacina->dr->st->dr && isdigit(radacina->dr->st->dr->info[0]))
                                            radacina->dr->st=radacina->dr->st->dr;
                                    calcule(radacina);
                                    arb1=new nod{"~",radacina->st,nullptr};
                                    radacina->st=arb1;
                                }
                                else
                                    if(radacina->st->dr && radacina->st->dr->info=="~" && radacina->st->st && (radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]) || radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0])))
                                    {
                                        nod *arb1;
                                        if(radacina->st->dr->st && isdigit(radacina->st->dr->st->info[0]))
                                            radacina->st->dr=radacina->st->dr->st;
                                        else
                                            if(radacina->st->dr->dr && isdigit(radacina->st->dr->dr->info[0]))
                                                radacina->st->dr=radacina->st->dr->dr;   
                                        calcule(radacina);
                                        arb1=new nod{"~",radacina->st,nullptr};
                                        radacina->st=arb1;
                                    }
                                        else
                                            if(radacina->dr->dr && radacina->dr->dr->info=="~" && radacina->dr->st && (radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]) || radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0])))
                                            {
                                                nod *arb1;
                                                if(radacina->dr->dr->st && isdigit(radacina->dr->dr->st->info[0]))
                                                    radacina->dr->dr=radacina->dr->dr->st;
                                                else
                                                    if(radacina->dr->dr->dr && isdigit(radacina->dr->dr->dr->info[0]))
                                                        radacina->dr->dr=radacina->dr->dr->dr;
                                                calcule(radacina);
                                                arb1=new nod{"~",radacina->st,nullptr};
                                                radacina->st=arb1;
                                            }
                            
        }
    }
    else
    {
        if(radacina->info=="+")
    {
        int is;
        double ds;
        string p;
        bool stg,drt;
        stg=drt=false;

        inordine(radacina->st);
        if(gasire_punct==true)
        {
            stg=true;
            if(radacina->st->info=="~")
            {
                double abc;

                if(radacina->st->st)
                    abc=stod(radacina->st->st->info)*(-1);
                else
                    abc=stod(radacina->st->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->st->info=dpp;
                radacina->st->st=nullptr;
            }
        }
        else
            if(radacina->st->info=="~")
                {
                    int abc;
                    string dpp;

                    if(radacina->st->st)
                        abc=stoi(radacina->st->st->info)*(-1);
                    else
                        abc=stoi(radacina->st->dr->info)*(-1);
                    ostringstream oss;
                    oss<<setprecision(10)<<abc;
                    dpp=oss.str();
                    radacina->st->info=dpp;
                    radacina->st->st=nullptr;
                }

        gasire_punct=false;
        inordine(radacina->dr);
        if(gasire_punct==true)
        {
            drt=true;
            if(radacina->dr->info=="~")
            {
                double abc;

                if(radacina->dr->st)
                    abc=stod(radacina->dr->st->info)*(-1);
                else
                    abc=stod(radacina->dr->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        }
        else
            if(radacina->dr->info=="~")
            {
                int abc;
                string dpp;

                if(radacina->dr->st)
                    abc=stoi(radacina->dr->st->info)*(-1);
                else
                    abc=stoi(radacina->dr->dr->info)*(-1);
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        
        if(stg==true)
            ds=stod(radacina->st->info);
        else
            is=stoi(radacina->st->info);

        if((drt==true || drt==false) && stg==true)
        {
            ds=ds+stod(radacina->dr->info);
            ostringstream oss;
            oss<<setprecision(10)<<ds;
            p=oss.str();
        }

        if(drt==true && stg==false)
        {
            ds=is+stod(radacina->dr->info);
            ostringstream oss;
            oss<<setprecision(10)<<ds;
            p=oss.str();
        } 

        if(drt==false && stg==false)
        {
            is=is+stoi(radacina->dr->info);
            p=to_string(is);
        }

        radacina->info=p;
        radacina->st=radacina->dr=nullptr;
    }
    else
        if(radacina->info=="-")
        {
            int is;
            double ds;
            string p;
            bool stg,drt;
            stg=drt=false;

            inordine(radacina->st);
            if(gasire_punct==true)
        {
            stg=true;
            if(radacina->st->info=="~")
            {
                double abc;

                if(radacina->st->st)
                    abc=stod(radacina->st->st->info)*(-1);
                else
                    abc=stod(radacina->st->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->st->info=dpp;
                radacina->st->st=nullptr;
            }
        }
        else
            if(radacina->st->info=="~")
                {
                    int abc;
                    string dpp;

                    if(radacina->st->st)
                        abc=stoi(radacina->st->st->info)*(-1);
                    else
                        abc=stoi(radacina->st->dr->info)*(-1);
                    ostringstream oss;
                    oss<<setprecision(10)<<abc;
                    dpp=oss.str();
                    radacina->st->info=dpp;
                    radacina->st->st=nullptr;
                }

        gasire_punct=false;
        inordine(radacina->dr);
        if(gasire_punct==true)
        {
            drt=true;
            if(radacina->dr->info=="~")
            {
                double abc;

                if(radacina->dr->st)
                    abc=stod(radacina->dr->st->info)*(-1);
                else
                    abc=stod(radacina->dr->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        }
        else
            if(radacina->dr->info=="~")
            {
                int abc;
                string dpp;

                if(radacina->dr->st)
                    abc=stoi(radacina->dr->st->info)*(-1);
                else
                    abc=stoi(radacina->dr->dr->info)*(-1);
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        
   
            if(stg==true)
                ds=stod(radacina->st->info);
            else
                is=stoi(radacina->st->info);

            if((drt==true || drt==false) && stg==true)
            {
                ds=ds-stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            }

            if(drt==true && stg==false)
            {
                ds=is-stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            } 

            if(drt==false && stg==false)
            {
                is=is-stoi(radacina->dr->info);
                p=to_string(is);
            }

            radacina->info=p;
            radacina->st=radacina->dr=nullptr;
    }
    else
        if(radacina->info=="*")
        {
            int is;
            double ds;
            string p;
            bool stg,drt;
            stg=drt=false;

            inordine(radacina->st);
            if(gasire_punct==true)
        {
            stg=true;
            if(radacina->st->info=="~")
            {
                double abc;

                if(radacina->st->st)
                    abc=stod(radacina->st->st->info)*(-1);
                else
                    abc=stod(radacina->st->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->st->info=dpp;
                radacina->st->st=nullptr;
            }
        }
        else
            if(radacina->st->info=="~")
                {
                    int abc;
                    string dpp;

                    if(radacina->st->st)
                        abc=stoi(radacina->st->st->info)*(-1);
                    else
                        abc=stoi(radacina->st->dr->info)*(-1);
                    ostringstream oss;
                    oss<<setprecision(10)<<abc;
                    dpp=oss.str();
                    radacina->st->info=dpp;
                    radacina->st->st=nullptr;
                }

        gasire_punct=false;
        inordine(radacina->dr);
        if(gasire_punct==true)
        {
            drt=true;
            if(radacina->dr->info=="~")
            {
                double abc;

                if(radacina->dr->st)
                    abc=stod(radacina->dr->st->info)*(-1);
                else
                    abc=stod(radacina->dr->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        }
        else
            if(radacina->dr->info=="~")
            {
                int abc;
                string dpp;

                if(radacina->dr->st)
                    abc=stoi(radacina->dr->st->info)*(-1);
                else
                    abc=stoi(radacina->dr->dr->info)*(-1);
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        
   
            if(stg==true)
                ds=stod(radacina->st->info);
            else
                is=stoi(radacina->st->info);

            if((drt==true || drt==false) && stg==true)
            {
                ds=ds*stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            }

            if(drt==true && stg==false)
            {
                ds=is*stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            } 

            if(drt==false && stg==false)
            {
                is=is*stoi(radacina->dr->info);
                p=to_string(is);
            }

            radacina->info=p;
            radacina->st=radacina->dr=nullptr;
    }
    else
        if(radacina->info=="/")
        {
            int is,dvz=0;
            double ds;
            string p,q;
            bool stg,drt;
            stg=drt=false;

            inordine(radacina->st);
            if(gasire_punct==true)
        {
            stg=true;
            if(radacina->st->info=="~")
            {
                double abc;

                if(radacina->st->st)
                    abc=stod(radacina->st->st->info)*(-1);
                else
                    abc=stod(radacina->st->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->st->info=dpp;
                radacina->st->st=nullptr;
            }
        }
        else
            if(radacina->st->info=="~")
                {
                    int abc;
                    string dpp;

                    if(radacina->st->st)
                        abc=stoi(radacina->st->st->info)*(-1);
                    else
                        abc=stoi(radacina->st->dr->info)*(-1);
                    ostringstream oss;
                    oss<<setprecision(10)<<abc;
                    dpp=oss.str();
                    radacina->st->info=dpp;
                    radacina->st->st=nullptr;
                }

        gasire_punct=false;
        inordine(radacina->dr);
        if(gasire_punct==true)
        {
            drt=true;
            if(radacina->dr->info=="~")
            {
                double abc;

                if(radacina->dr->st)
                    abc=stod(radacina->dr->st->info)*(-1);
                else
                    abc=stod(radacina->dr->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        }
        else
            if(radacina->dr->info=="~")
            {
                int abc;
                string dpp;

                if(radacina->dr->st)
                    abc=stoi(radacina->dr->st->info)*(-1);
                else
                    abc=stoi(radacina->dr->dr->info)*(-1);
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        
   
            if(stg==true)
                ds=stod(radacina->st->info);
            else
                is=stoi(radacina->st->info);

            if((drt==true || drt==false) && stg==true)
            {
                ds=ds/stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            }

            if(drt==true && stg==false)
            {
                ds=is/stod(radacina->dr->info);
                ostringstream oss;
                oss<<setprecision(10)<<ds;
                p=oss.str();
            } 

            if(drt==false && stg==false)
            {
                int abb;
                abb=stoi(radacina->dr->info);
                if(is%abb==0)
                    is=is/stoi(radacina->dr->info);
                else
                    {   
                        dvz=cmmdc(is,abb);
                        is=is/dvz;
                        abb=abb/dvz;

                        q=to_string(abb);
                        
                    }
                p=to_string(is);
            }

            if(drt==false && stg==false && dvz!=0)
            {
                radacina->st->info=p;
                radacina->dr->info=q;
            }
            else
            {
                radacina->info=p;
                radacina->st=radacina->dr=nullptr;
            }
    }
    else
        if(radacina->info=="^")
        {
            int is;
            double ds;
            string p;
            bool stg,drt;
            stg=drt=false;

            inordine(radacina->st);
            if(gasire_punct==true)
        {
            stg=true;
            if(radacina->st->info=="~")
            {
                double abc;

                if(radacina->st->st)
                    abc=stod(radacina->st->st->info)*(-1);
                else
                    abc=stod(radacina->st->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->st->info=dpp;
                radacina->st->st=nullptr;
            }
        }
        else
            if(radacina->st->info=="~")
                {
                    int abc;
                    string dpp;

                    if(radacina->st->st)
                        abc=stoi(radacina->st->st->info)*(-1);
                    else
                        abc=stoi(radacina->st->dr->info)*(-1);
                    ostringstream oss;
                    oss<<setprecision(10)<<abc;
                    dpp=oss.str();
                    radacina->st->info=dpp;
                    radacina->st->st=nullptr;
                }

        gasire_punct=false;
        inordine(radacina->dr);
        if(gasire_punct==true)
        {
            drt=true;
            if(radacina->dr->info=="~")
            {
                double abc;

                if(radacina->dr->st)
                    abc=stod(radacina->dr->st->info)*(-1);
                else
                    abc=stod(radacina->dr->dr->info)*(-1);
                string dpp;
                
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        }
        else
            if(radacina->dr->info=="~")
            {
                int abc;
                string dpp;

                if(radacina->dr->st)
                    abc=stoi(radacina->dr->st->info)*(-1);
                else
                    abc=stoi(radacina->dr->dr->info)*(-1);
                ostringstream oss;
                oss<<setprecision(10)<<abc;
                dpp=oss.str();
                radacina->dr->info=dpp;
                radacina->dr->st=nullptr;
            }
        
   
            if(stg==true)
                ds=stod(radacina->st->info);
            else
                is=stoi(radacina->st->info);

            if((drt==true || drt==false) && stg==true)
            {
                double pp;
                pp=pow(ds,stod(radacina->dr->info));
                ostringstream oss;
                oss<<setprecision(10)<<pp;
                p=oss.str();
            }

            if(drt==true && stg==false)
            {
                double pp;
                pp=pow(is,stod(radacina->dr->info));
                ostringstream oss;
                oss<<setprecision(10)<<pp;
                p=oss.str();
            } 

            if(drt==false && stg==false)
            {
                int pp;
                pp=pow(is,stoi(radacina->dr->info));
                p=to_string(pp);
            }

            radacina->info=p;
            radacina->st=radacina->dr=nullptr;
    }
    }
}


int main()
{
    bool opuu=false;
    int ctexp;
    char s[256],v[256][256];
    cin.get(s,256);
    creare_vector_de_cuvinte(s,v,ctexp);
    if(verificare_expresie(v,ctexp)==1)
    {
        forma_postfixata(v,ctexp);
        arborizare();
        calcule(arbore);
        simplifica(arbore);
        derivare(arbore,arbore_dervivat);
        simplifica(arbore_dervivat);
        calcule(arbore_dervivat);
        simplifica(arbore_dervivat);
        if(eroare==true)
            return 0;
        afisare(arbore_dervivat);
        //afisare(arbore);
    }
    else
        cout<<"Expresia este incorecta";

    return 0;
}