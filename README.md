## <img src="trasnparent-1.png"  style="margin: 0px 50px -5px 10px">The Monkey Compiler 

The Monkey Compiler is a small open sourced compiling system. The goal of this project is to assist understanding the phases in compiling functional programming languages. You can explore the compiling procedures and intermediate representations by specifying and examining the output. In particular, the Monkey Compiler provides two parallel methods to eliminate higher-order functions, which may reveal some aspects to improve the efficiency of the target program and facilitate further study.

###Source Language

The source language of Monkey Compiler is basically a subset of Standard ML. Chapter 1 (CPS Conversion) of the document describes the abstract syntax of the source language (basically the same with the concrete syntax). Chapter 0 (Front End) also provides some details about source language syntax.

##release 1.0

###Structure

The figure below descirbes the basic structure of the compiler. 

<*pic*>

###How to use

Currently the executive version of the compiler for Windows(64bit) is provided (generated by MLton). You may also compile the source code to other platform using tool such as MLton or SML/NJ. The Monkey Compiler cannot compile itself for now. 

<*lib file*>

Once you have the executive version of the Monkey Compiler, you may run it with command (given that the compiler's executive file is named "monkey", and it can be found in Path)

`monkey [option ...] filename`

###Options

The following options are supported:

- `-defunc` 

	Use the defunctionalization strategy for higher-order function elimination. This is the default setting.

- `-closure`

	Use closure conversion strategy for higher-order function elimination. 

- `-all` 

	Output the codes of all the intermediate representations. Note that the output for CPS representaion, closure passing representaion and defunctionalized representaion (all ends with ".sml") cannot be compiled and run, for SML compilers cannot infer the type information.

- `-t`

	The executive file you generated will print its run time to the console. 

<*execltive? gcc? clang?*>

Multiple options can be selected.
    
<br/><br/><br/>