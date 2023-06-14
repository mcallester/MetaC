/** ========================================================================
macros needed for objects file
========================================================================**/

deflists(expptr);

deflists(voidptr);

umacro{pushprop($val, getprop($x, $prop))}{
  expptr xval = gensym(`xval);
  expptr propval = gensym(`prop);
  return `{{
      voidptr $xval = $x;
      voidptr $propval = $prop;
      setprop($xval, $propval, voidptr_cons($val, (voidptr_list) getprop($xval, $propval, NULL)));}};
  }

umacro{class_dim()}{return `100;}
