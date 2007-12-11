;;;;;;; Teste RayTracing in Phase ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO testrt

phainit

pset={PHASEset}
bltype={BeamlineType}


phaInitPHASEset,pset
phaSrcRTGauss,pset,bltype
print,pset

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO phaInitPHASEset,PHASEset

;phaInitPHASEset(struct PHASEset *x)

result = call_external(!phalib,'phaInitPHASEset' $
		       , PHASEset $  
                       , /UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO phaSrcRTGauss,PHASEset,BeamlineType 

;beam.fsource4a(0:strlen(name+'_a')-1) = byte(name+'_a') 

BeamlineType.RTSource.QuellType=byte('o')

result = call_external(!phalib,'MakeRTSource' $
		       , PHASEset,BeamlineType $  
                       , /UNLOAD,/CDECL,/AUTO_GLUE,/IGNORE_EXISTING_GLUE)

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
