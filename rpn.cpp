/*Scott Brunstein*/
//rpn.cpp
//Create a Reverse Polish Notation Calculator

#include<iostream>
#include<stack>
#include<string>
#include<sstream>
#include<iomanip>
#include<typeinfo>
#include<cstdlib>
#include<iterator>
using namespace std;

//Function operatorCheck.
//Checks if a character that isn't a double is a valid operator. If it is, return 1, if not, return 0.
bool operatorCheck(const string& val)
{
  if (val == "+")
  {  return true; }
  else if (val == "-")
  { return true; }
  else if (val == "*")
  { return true; }
  else if (val == "/")
  { return true; }
  else
  { return false; }
}

//Function operatorComp.
//If operatorCheck and checkStack both return 1, call this function to perform the computation
//with the correct operator.
void operatorComp(const string& val, stack<double>& v)
{
  double v1, v2, vtotal;
  v2 = v.top();
  v.pop();
  v1 = v.top();
  v.pop();
  if (val == "+")
  { vtotal = v1 + v2; v.push(vtotal);}
  else if (val == "-")
  { vtotal = v1 - v2; v.push(vtotal);}
  else if (val == "*")
  { vtotal = v1 * v2; v.push(vtotal);}
  else if (val == "/")
  {
    if (v2 == 0)
    {
      cout << "division by zero" << endl;
      exit(0);
    } 
    else {vtotal = v1 / v2; v.push(vtotal);}
  }
}

//Function checkStack.
//Called if operatorCheck returns 1. If the size is >= 2, return 1. Else, return 0.
//If this funciton returns 0, print 'stack underflow'.
bool checkStack(stack<double>& v)
{
  if (v.size() >= 2)
  {
    return true;
  }
  else
  {
    //return false;
    cout << "stack underflow" << endl;
    exit(0);
  }
}

//Function checkString
//Called to determine if a number has any proceeding characters. Exits program on invalid input if true.
bool checkString(const string& val)
{ 
  size_t invalid = val.find_first_not_of("0123456789+-*/. ");
  if (invalid != string::npos)
    {return true;}
  else
  { return false;}
}

int main()
{
  /*Initializing variables and stack*/
  stack<double> v;
  double tmp;
  string tmp2;
  string val;

  while (true) //enter infinite loop to loop through input
  {
    cin >> val;
    if(!cin)
    { 
      for (int i = 0; i <= v.size(); i++)
      {
        cout << v.top() << endl;
        v.pop();
      }
      exit(0);
    }	
    istringstream is(val);
    if ( is >> tmp ) //use stringstream fail() to check this for 5asdf
      if (checkString(val) == 1)
      {
        cout << "invalid input" << endl;
        exit(0);
      }
      else
      {
          v.push(tmp);
      }
    else if (operatorCheck(val) == 1)
    {
      if ( checkStack(v) == 1 )
      {
        operatorComp(val, v);
      }
    }
    else
    {
      cout << "invalid input" << endl;
      exit(0);
    }
  }
}
