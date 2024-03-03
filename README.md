### A step mode interrupt (int 1) handler that recognizes the ADC reg+r/m command. 16 bit x86 Assembly.

The program checks whether an interruption occurred before executing the first option of the ADC command, if so, it displays a warning on the screen and all of the information about the command: 

address, code, mnemonic, operands.

Example: The information displayed on the screen could look like this: 

Interruption of step mode! 0000:0128 12C6 adc al, dh; al= 00, dh= 11.
