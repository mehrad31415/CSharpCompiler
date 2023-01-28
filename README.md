# CSharpCompiler
*A code generator for a subset of C# which converts the code to SSM language (simple stack machine), a low level Assembly language.*

##### The program is written in Haskell using the parser combinators library i.e., using the package uu-tc.

##### The source codes has six files:

* CSharpLex.hs: a lexical scanner for C# that transforms flat input (a string) into a list of tokens.
* CSharpGram.hs: types and functions for parsing C#.
* CSharpAlgebra.hs: The algebra type for the C# AST, and a corresponding fold
function.
* SSM.hs: types and utilities for representing SSM programs.
* CSharpCode.hs: the code generator as an algebra, to transform C# abstract syntax into SSM code.
* Main.hs: main program that contains a driver calling the different phases in the right order. The program reads a C# file and writes an SSM result.
* ssmui.jar/ssm.jar: graphical simulator for the SSM. With this, you can run the generated code and test whether your code generator is working correctly.
* ssm.bat/ssm.sh/ssm2.bat/ssm2.sh: wrapper script to call ssmui.jar/ssm.jar with the generated code.

##### One example code that it can parse is as follows:

```
class Hello
{
    int g;
    
    void main()
    {
        int b;
        b = 1;
    }
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;   
    }

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
   }
}

```

For further information on the details please read the pdf
