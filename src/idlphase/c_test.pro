;c_test.pro


function ntest,beam


result = call_external(!phalib,'test',$
			beam,$
			/D_VALUE,/UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)
                        ;,WRITE_WRAPPER='test.tst')

return,result
end
