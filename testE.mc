expptr load(expptr forms){ // forms must both be fully macro expanded.

  compilecount ++; //avoids argument duplication problem with sformat
  char * s = sformat("TEMP%d.c",compilecount);
}
