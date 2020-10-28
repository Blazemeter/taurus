#!/bin/bash

cat <<EOF > $TAURUS_ARTIFACTS_DIR/NUnitExecutor.ldjson
{"start_time":1501590114,"duration":0.011648,"test_case":"ErroringTest","test_suite":"NUnitSuite.Test","status":"FAILED","error_msg":"System.Exception : Ima error","error_trace":"at NUnitSuite.Test.ErroringTest () <0x401e3ee0 + 0x0002f> in <filename unknown>:0 \n  at (wrapper managed-to-native) System.Reflection.MonoMethod:InternalInvoke (System.Reflection.MonoMethod,object,object[],System.Exception&)\n  at System.Reflection.MonoMethod.Invoke (System.Object obj, BindingFlags invokeAttr, System.Reflection.Binder binder, System.Object[] parameters, System.Globalization.CultureInfo culture) <0x401bfe00 + 0x000b7> in <filename unknown>:0","extras":null}
{"start_time":1501590114,"duration":0.020751,"test_case":"FailingTest","test_suite":"NUnitSuite.Test","status":"FAILED","error_msg":"Expected: 6\n  But was:  8","error_trace":"at NUnitSuite.Test.FailingTest () <0x401ee190 + 0x0005b> in <filename unknown>:0","extras":null}
{"start_time":1501590114,"duration":0.000577,"test_case":"PassingTest","test_suite":"NUnitSuite.Test","status":"PASSED","error_msg":"","error_trace":"","extras":null}
{"start_time":1501590114,"duration":0.000627,"test_case":"SkippedTest","test_suite":"NUnitSuite.Test","status":"SKIPPED","error_msg":"Skippy","error_trace":"","extras":null}
EOF

