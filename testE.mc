expptr load(expptr forms){ // forms must both be fully macro expanded.

  compilecount ++; //avoids argument duplication problem with eformat
  char * s = eformat("TEMP%d.c",compilecount);
}
