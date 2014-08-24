module TestLensExamples where

import BinaryList
import LensCheck
import LensExamples
import EasyCheck

dateLensGetPut :: IO ()
dateLensGetPut = easyCheck1 (checkGetPut dateLens)

dateLensPutGet :: IO ()
dateLensPutGet = easyCheck2 (checkPutGet dateLens)

dateLensPutPut :: IO ()
dateLensPutPut = easyCheck3 (checkPutPut dateLens)

dateLensPutDet :: IO ()
dateLensPutDet = easyCheck2 (checkPutDet dateLens)

dateLensPutStab :: IO ()
dateLensPutStab = easyCheck1 (checkPutStab dateLens)

monthLensGetPut :: IO ()
monthLensGetPut = easyCheck1 (checkGetPut monthLens)

monthLensPutGet :: IO ()
monthLensPutGet = easyCheck2 (checkPutGet monthLens)

monthLensPutPut :: IO ()
monthLensPutPut = easyCheck3 (checkPutPut monthLens)

monthLensPutDet :: IO ()
monthLensPutDet = easyCheck2 (checkPutDet monthLens)

monthLensPutStab :: IO ()
monthLensPutStab = easyCheck1 (checkPutStab monthLens)

dayLensGetPut :: IO ()
dayLensGetPut = easyCheck1 (checkGetPut dayLens)

dayLensPutGet :: IO ()
dayLensPutGet = easyCheck2 (checkPutGet dayLens)

dayLensPutPut :: IO ()
dayLensPutPut = easyCheck3 (checkPutPut dayLens)

dayLensPutDet :: IO ()
dayLensPutDet = easyCheck2 (checkPutDet dayLens)

dayLensPutStab :: IO ()
dayLensPutStab = easyCheck1 (checkPutStab dayLens)

addressLensGetPut :: IO ()
addressLensGetPut = easyCheck1 (checkGetPut addressLens)

addressLensPutGet :: IO ()
addressLensPutGet = easyCheck2 (checkPutGet addressLens)

addressLensPutPut :: IO ()
addressLensPutPut = easyCheck3 (checkPutPut addressLens)

addressLensPutDet :: IO ()
addressLensPutDet = easyCheck2 (checkPutDet addressLens)

addressLensPutStab :: IO ()
addressLensPutStab = easyCheck1 (checkPutStab addressLens)

nameLensGetPut :: IO ()
nameLensGetPut = easyCheck1 (checkGetPut nameLens)

nameLensPutGet :: IO ()
nameLensPutGet = easyCheck2 (checkPutGet nameLens)

nameLensPutPut :: IO ()
nameLensPutPut = easyCheck3 (checkPutPut nameLens)

nameLensPutDet :: IO ()
nameLensPutDet = easyCheck2 (checkPutDet nameLens)

nameLensPutStab :: IO ()
nameLensPutStab = easyCheck1 (checkPutStab nameLens)

falsePutGetPut :: IO ()
falsePutGetPut = easyCheck1 (checkGetPut falsePut)

falsePutPutGet :: IO ()
falsePutPutGet = easyCheck2 (checkPutGet falsePut)

falsePutPutPut :: IO ()
falsePutPutPut = easyCheck3 (checkPutPut falsePut)

falsePutPutDet :: IO ()
falsePutPutDet = easyCheck2 (checkPutDet falsePut)

falsePutPutStab :: IO ()
falsePutPutStab = easyCheck1 (checkPutStab falsePut)

putHoursGetPut :: IO ()
putHoursGetPut = easyCheck1 (checkGetPut putHours)

putHoursPutGet :: IO ()
putHoursPutGet = easyCheck2 (checkPutGet putHours)

putHoursPutPut :: IO ()
putHoursPutPut = easyCheck3 (checkPutPut putHours)

putHoursPutDet :: IO ()
putHoursPutDet = easyCheck2 (checkPutDet putHours)

putHoursPutStab :: IO ()
putHoursPutStab = easyCheck1 (checkPutStab putHours)

putMinsGetPut :: IO ()
putMinsGetPut = easyCheck1 (checkGetPut putMins)

putMinsPutGet :: IO ()
putMinsPutGet = easyCheck2 (checkPutGet putMins)

putMinsPutPut :: IO ()
putMinsPutPut = easyCheck3 (checkPutPut putMins)

putMinsPutDet :: IO ()
putMinsPutDet = easyCheck2 (checkPutDet putMins)

putMinsPutStab :: IO ()
putMinsPutStab = easyCheck1 (checkPutStab putMins)

putHalveBinaryListGetPut :: IO ()
putHalveBinaryListGetPut = easyCheck1 (checkGetPut putHalveBinaryList)

putHalveBinaryListPutGet :: IO ()
putHalveBinaryListPutGet = easyCheck2 (checkPutGet putHalveBinaryList)

putHalveBinaryListPutPut :: IO ()
putHalveBinaryListPutPut = easyCheck3 (checkPutPut putHalveBinaryList)

putHalveBinaryListPutDet :: IO ()
putHalveBinaryListPutDet = easyCheck2 (checkPutDet putHalveBinaryList)

putHalveBinaryListPutStab :: IO ()
putHalveBinaryListPutStab = easyCheck1 (checkPutStab putHalveBinaryList)

putHalvePeanoGetPut :: IO ()
putHalvePeanoGetPut = easyCheck (checkListGetPut putHalvePeano)

putHalvePeanoPutGet :: IO ()
putHalvePeanoPutGet = easyCheck1 (checkListPutGet putHalvePeano)

putHalvePeanoPutPut :: IO ()
putHalvePeanoPutPut = easyCheck2 (checkListPutPut putHalvePeano)

putHalvePeanoPutDet :: IO ()
putHalvePeanoPutDet = easyCheck1 (checkListPutDet putHalvePeano)

putHalvePeanoPutStab :: IO ()
putHalvePeanoPutStab = easyCheck (checkListPutStab putHalvePeano)

putflattenGetPut :: IO ()
putflattenGetPut = easyCheck1 (checkGetPut putflatten)

putflattenPutGet :: IO ()
putflattenPutGet = easyCheck2 (checkPutGet putflatten)

putflattenPutPut :: IO ()
putflattenPutPut = easyCheck3 (checkPutPut putflatten)

putflattenPutDet :: IO ()
putflattenPutDet = easyCheck2 (checkPutDet putflatten)

putflattenPutStab :: IO ()
putflattenPutStab = easyCheck1 (checkPutStab putflatten)

putFirstGetPut :: IO ()
putFirstGetPut = easyCheck1 (checkGetPut putFirst)

putFirstPutGet :: IO ()
putFirstPutGet = easyCheck2 (checkPutGet putFirst)

putFirstPutPut :: IO ()
putFirstPutPut = easyCheck3 (checkPutPut putFirst)

putFirstPutDet :: IO ()
putFirstPutDet = easyCheck2 (checkPutDet putFirst)

putFirstPutStab :: IO ()
putFirstPutStab = easyCheck1 (checkPutStab putFirst)

putSecondGetPut :: IO ()
putSecondGetPut = easyCheck1 (checkGetPut putSecond)

putSecondPutGet :: IO ()
putSecondPutGet = easyCheck2 (checkPutGet putSecond)

putSecondPutPut :: IO ()
putSecondPutPut = easyCheck3 (checkPutPut putSecond)

putSecondPutDet :: IO ()
putSecondPutDet = easyCheck2 (checkPutDet putSecond)

putSecondPutStab :: IO ()
putSecondPutStab = easyCheck1 (checkPutStab putSecond)

putFirstCountGetPut :: IO ()
putFirstCountGetPut = easyCheck1 (checkGetPut putFirstCount)

putFirstCountPutGet :: IO ()
putFirstCountPutGet = easyCheck2 (checkPutGet putFirstCount)

putFirstCountPutPut :: IO ()
putFirstCountPutPut = easyCheck3 (checkPutPut putFirstCount)

putFirstCountPutDet :: IO ()
putFirstCountPutDet = easyCheck2 (checkPutDet putFirstCount)

putFirstCountPutStab :: IO ()
putFirstCountPutStab = easyCheck1 (checkPutStab putFirstCount)

putFirstDiffGetPut :: IO ()
putFirstDiffGetPut = easyCheck1 (checkGetPut putFirstDiff)

putFirstDiffPutGet :: IO ()
putFirstDiffPutGet = easyCheck2 (checkPutGet putFirstDiff)

putFirstDiffPutPut :: IO ()
putFirstDiffPutPut = easyCheck3 (checkPutPut putFirstDiff)

putFirstDiffPutDet :: IO ()
putFirstDiffPutDet = easyCheck2 (checkPutDet putFirstDiff)

putFirstDiffPutStab :: IO ()
putFirstDiffPutStab = easyCheck1 (checkPutStab putFirstDiff)

cityGetPut :: IO ()
cityGetPut = easyCheck1 (checkGetPut city)

cityPutGet :: IO ()
cityPutGet = easyCheck2 (checkPutGet city)

cityPutPut :: IO ()
cityPutPut = easyCheck3 (checkPutPut city)

cityPutDet :: IO ()
cityPutDet = easyCheck2 (checkPutDet city)

cityPutStab :: IO ()
cityPutStab = easyCheck1 (checkPutStab city)

nameGetPut :: IO ()
nameGetPut = easyCheck1 (checkGetPut name)

namePutGet :: IO ()
namePutGet = easyCheck2 (checkPutGet name)

namePutPut :: IO ()
namePutPut = easyCheck3 (checkPutPut name)

namePutDet :: IO ()
namePutDet = easyCheck2 (checkPutDet name)

namePutStab :: IO ()
namePutStab = easyCheck1 (checkPutStab name)

putAsGetPut :: IO ()
putAsGetPut = easyCheck (checkListGetPut putAs)

putAsPutGet :: IO ()
putAsPutGet = easyCheck1 (checkListPutGet putAs)

putAsPutPut :: IO ()
putAsPutPut = easyCheck2 (checkListPutPut putAs)

putAsPutDet :: IO ()
putAsPutDet = easyCheck1 (checkListPutDet putAs)

putAsPutStab :: IO ()
putAsPutStab = easyCheck (checkListPutStab putAs)

putStudentsGetPut :: IO ()
putStudentsGetPut = easyCheck (checkListGetPut putStudents)

putStudentsPutGet :: IO ()
putStudentsPutGet = easyCheck1 (checkListPutGet putStudents)

putStudentsPutPut :: IO ()
putStudentsPutPut = easyCheck2 (checkListPutPut putStudents)

putStudentsPutDet :: IO ()
putStudentsPutDet = easyCheck1 (checkListPutDet putStudents)

putStudentsPutStab :: IO ()
putStudentsPutStab = easyCheck (checkListPutStab putStudents)

putHeadGetPut :: IO ()
putHeadGetPut = easyCheck (checkListGetPut putHead)

putHeadPutGet :: IO ()
putHeadPutGet = easyCheck1 (checkListPutGet putHead)

putHeadPutPut :: IO ()
putHeadPutPut = easyCheck2 (checkListPutPut putHead)

putHeadPutDet :: IO ()
putHeadPutDet = easyCheck1 (checkListPutDet putHead)

putHeadPutStab :: IO ()
putHeadPutStab = easyCheck (checkListPutStab putHead)

putTailGetPut :: IO ()
putTailGetPut = easyCheck (checkListGetPut putTail)

putTailPutGet :: IO ()
putTailPutGet = easyCheck1 (checkListPutGet putTail)

putTailPutPut :: IO ()
putTailPutPut = easyCheck2 (checkListPutPut putTail)

putTailPutDet :: IO ()
putTailPutDet = easyCheck1 (checkListPutDet putTail)

putTailPutStab :: IO ()
putTailPutStab = easyCheck (checkListPutStab putTail)

putTakeGetPut :: IO ()
putTakeGetPut = easyCheck (checkListGetPut putTake)

putTakePutGet :: IO ()
putTakePutGet = easyCheck1 (checkListPutGet putTake)

putTakePutPut :: IO ()
putTakePutPut = easyCheck2 (checkListPutPut putTake)

putTakePutDet :: IO ()
putTakePutDet = easyCheck1 (checkListPutDet putTake)

putTakePutStab :: IO ()
putTakePutStab = easyCheck (checkListPutStab putTake)

putZipGetPut :: IO ()
putZipGetPut = easyCheck1 (checkGetPut putZip)

putZipPutGet :: IO ()
putZipPutGet = easyCheck2 (checkPutGet putZip)

putZipPutPut :: IO ()
putZipPutPut = easyCheck3 (checkPutPut putZip)

putZipPutDet :: IO ()
putZipPutDet = easyCheck2 (checkPutDet putZip)

putZipPutStab :: IO ()
putZipPutStab = easyCheck1 (checkPutStab putZip)

putZip'GetPut :: IO ()
putZip'GetPut = easyCheck1 (checkGetPut putZip')

putZip'PutGet :: IO ()
putZip'PutGet = easyCheck2 (checkPutGet putZip')

putZip'PutPut :: IO ()
putZip'PutPut = easyCheck3 (checkPutPut putZip')

putZip'PutDet :: IO ()
putZip'PutDet = easyCheck2 (checkPutDet putZip')

putZip'PutStab :: IO ()
putZip'PutStab = easyCheck1 (checkPutStab putZip')

putAltGetPut :: IO ()
putAltGetPut = easyCheck (checkListGetPut putAlt)

putAltPutGet :: IO ()
putAltPutGet = easyCheck1 (checkListPutGet putAlt)

putAltPutPut :: IO ()
putAltPutPut = easyCheck2 (checkListPutPut putAlt)

putAltPutDet :: IO ()
putAltPutDet = easyCheck1 (checkListPutDet putAlt)

putAltPutStab :: IO ()
putAltPutStab = easyCheck (checkListPutStab putAlt)

putAppendGetPut :: IO ()
putAppendGetPut = easyCheck1 (checkGetPut putAppend)

putAppendPutGet :: IO ()
putAppendPutGet = easyCheck2 (checkPutGet putAppend)

putAppendPutPut :: IO ()
putAppendPutPut = easyCheck3 (checkPutPut putAppend)

putAppendPutDet :: IO ()
putAppendPutDet = easyCheck2 (checkPutDet putAppend)

putAppendPutStab :: IO ()
putAppendPutStab = easyCheck1 (checkPutStab putAppend)

putInitGetPut :: IO ()
putInitGetPut = easyCheck (checkListGetPut putInit)

putInitPutGet :: IO ()
putInitPutGet = easyCheck1 (checkListPutGet putInit)

putInitPutPut :: IO ()
putInitPutPut = easyCheck2 (checkListPutPut putInit)

putInitPutDet :: IO ()
putInitPutDet = easyCheck1 (checkListPutDet putInit)

putInitPutStab :: IO ()
putInitPutStab = easyCheck (checkListPutStab putInit)

putConsGetPut :: IO ()
putConsGetPut = easyCheck (checkListGetPut putCons)

putConsPutGet :: IO ()
putConsPutGet = easyCheck1 (checkListPutGet putCons)

putConsPutPut :: IO ()
putConsPutPut = easyCheck2 (checkListPutPut putCons)

putConsPutDet :: IO ()
putConsPutDet = easyCheck1 (checkListPutDet putCons)

putConsPutStab :: IO ()
putConsPutStab = easyCheck (checkListPutStab putCons)

putUnconsGetPut :: IO ()
putUnconsGetPut = easyCheck1 (checkGetPut putUncons)

putUnconsPutGet :: IO ()
putUnconsPutGet = easyCheck2 (checkPutGet putUncons)

putUnconsPutPut :: IO ()
putUnconsPutPut = easyCheck3 (checkPutPut putUncons)

putUnconsPutDet :: IO ()
putUnconsPutDet = easyCheck2 (checkPutDet putUncons)

putUnconsPutStab :: IO ()
putUnconsPutStab = easyCheck1 (checkPutStab putUncons)

filterLPutGetPut :: IO ()
filterLPutGetPut = easyCheck (checkListGetPut filterLPut)

filterLPutPutGet :: IO ()
filterLPutPutGet = easyCheck1 (checkListPutGet filterLPut)

filterLPutPutPut :: IO ()
filterLPutPutPut = easyCheck2 (checkListPutPut filterLPut)

filterLPutPutDet :: IO ()
filterLPutPutDet = easyCheck1 (checkListPutDet filterLPut)

filterLPutPutStab :: IO ()
filterLPutPutStab = easyCheck (checkListPutStab filterLPut)

putSumTreeShiftGetPut :: IO ()
putSumTreeShiftGetPut = easyCheck1 (checkGetPut putSumTreeShift)

putSumTreeShiftPutGet :: IO ()
putSumTreeShiftPutGet = easyCheck2 (checkPutGet putSumTreeShift)

putSumTreeShiftPutPut :: IO ()
putSumTreeShiftPutPut = easyCheck3 (checkPutPut putSumTreeShift)

putSumTreeShiftPutDet :: IO ()
putSumTreeShiftPutDet = easyCheck2 (checkPutDet putSumTreeShift)

putSumTreeShiftPutStab :: IO ()
putSumTreeShiftPutStab = easyCheck1 (checkPutStab putSumTreeShift)

putSumTreeDivNatGetPut :: IO ()
putSumTreeDivNatGetPut = easyCheck1 (checkGetPut putSumTreeDivNat)

putSumTreeDivNatPutGet :: IO ()
putSumTreeDivNatPutGet = easyCheck2 (checkPutGet putSumTreeDivNat)

putSumTreeDivNatPutPut :: IO ()
putSumTreeDivNatPutPut = easyCheck3 (checkPutPut putSumTreeDivNat)

putSumTreeDivNatPutDet :: IO ()
putSumTreeDivNatPutDet = easyCheck2 (checkPutDet putSumTreeDivNat)

putSumTreeDivNatPutStab :: IO ()
putSumTreeDivNatPutStab = easyCheck1 (checkPutStab putSumTreeDivNat)

mainTest = do
  putStrLn "\n+++++dateLensGetPut+++++"
  dateLensGetPut
  putStrLn "\n+++++dateLensPutGet+++++"
  dateLensPutGet
  putStrLn "\n+++++dateLensPutDet+++++"
  dateLensPutDet
  putStrLn "\n+++++dateLensPutStab+++++"
  dateLensPutStab
  putStrLn "\n+++++monthLensGetPut+++++"
  monthLensGetPut
  putStrLn "\n+++++monthLensPutGet+++++"
  monthLensPutGet
  putStrLn "\n+++++monthLensPutDet+++++"
  monthLensPutDet
  putStrLn "\n+++++monthLensPutStab+++++"
  monthLensPutStab
  putStrLn "\n+++++dayLensGetPut+++++"
  dayLensGetPut
  putStrLn "\n+++++dayLensPutGet+++++"
  dayLensPutGet
  putStrLn "\n+++++dayLensPutDet+++++"
  dayLensPutDet
  putStrLn "\n+++++dayLensPutStab+++++"
  dayLensPutStab
  putStrLn "\n+++++addressLensGetPut+++++"
  addressLensGetPut
  putStrLn "\n+++++addressLensPutGet+++++"
  addressLensPutGet
  putStrLn "\n+++++addressLensPutDet+++++"
  addressLensPutDet
  putStrLn "\n+++++addressLensPutStab+++++"
  addressLensPutStab
  putStrLn "\n+++++nameLensGetPut+++++"
  nameLensGetPut
  putStrLn "\n+++++nameLensPutGet+++++"
  nameLensPutGet
  putStrLn "\n+++++nameLensPutDet+++++"
  nameLensPutDet
  putStrLn "\n+++++nameLensPutStab+++++"
  nameLensPutStab
  putStrLn "\n+++++falsePutGetPut+++++"
  falsePutGetPut
  putStrLn "\n+++++falsePutPutGet+++++"
  falsePutPutGet
  putStrLn "\n+++++falsePutPutDet+++++"
  falsePutPutDet
  putStrLn "\n+++++falsePutPutStab+++++"
  falsePutPutStab
  putStrLn "\n+++++putHoursGetPut+++++"
  putHoursGetPut
  putStrLn "\n+++++putHoursPutGet+++++"
  putHoursPutGet
  putStrLn "\n+++++putHoursPutDet+++++"
  putHoursPutDet
  putStrLn "\n+++++putHoursPutStab+++++"
  putHoursPutStab
  putStrLn "\n+++++putMinsGetPut+++++"
  putMinsGetPut
  putStrLn "\n+++++putMinsPutGet+++++"
  putMinsPutGet
  putStrLn "\n+++++putMinsPutDet+++++"
  putMinsPutDet
  putStrLn "\n+++++putMinsPutStab+++++"
  putMinsPutStab
  putStrLn "\n+++++putHalveBinaryListGetPut+++++"
  putHalveBinaryListGetPut
  putStrLn "\n+++++putHalveBinaryListPutGet+++++"
  putHalveBinaryListPutGet
  putStrLn "\n+++++putHalveBinaryListPutDet+++++"
  putHalveBinaryListPutDet
  putStrLn "\n+++++putHalveBinaryListPutStab+++++"
  putHalveBinaryListPutStab
  putStrLn "\n+++++putHalvePeanoGetPut+++++"
  putHalvePeanoGetPut
  putStrLn "\n+++++putHalvePeanoPutGet+++++"
  putHalvePeanoPutGet
  putStrLn "\n+++++putHalvePeanoPutDet+++++"
  putHalvePeanoPutDet
  putStrLn "\n+++++putHalvePeanoPutStab+++++"
  putHalvePeanoPutStab
  putStrLn "\n+++++putflattenGetPut+++++"
  putflattenGetPut
  putStrLn "\n+++++putflattenPutGet+++++"
  putflattenPutGet
  putStrLn "\n+++++putflattenPutDet+++++"
  putflattenPutDet
  putStrLn "\n+++++putflattenPutStab+++++"
  putflattenPutStab
  putStrLn "\n+++++putFirstGetPut+++++"
  putFirstGetPut
  putStrLn "\n+++++putFirstPutGet+++++"
  putFirstPutGet
  putStrLn "\n+++++putFirstPutDet+++++"
  putFirstPutDet
  putStrLn "\n+++++putFirstPutStab+++++"
  putFirstPutStab
  putStrLn "\n+++++putSecondGetPut+++++"
  putSecondGetPut
  putStrLn "\n+++++putSecondPutGet+++++"
  putSecondPutGet
  putStrLn "\n+++++putSecondPutDet+++++"
  putSecondPutDet
  putStrLn "\n+++++putSecondPutStab+++++"
  putSecondPutStab
  putStrLn "\n+++++putFirstCountGetPut+++++"
  putFirstCountGetPut
  putStrLn "\n+++++putFirstCountPutGet+++++"
  putFirstCountPutGet
  putStrLn "\n+++++putFirstCountPutDet+++++"
  putFirstCountPutDet
  putStrLn "\n+++++putFirstCountPutStab+++++"
  putFirstCountPutStab
  putStrLn "\n+++++putFirstDiffGetPut+++++"
  putFirstDiffGetPut
  putStrLn "\n+++++putFirstDiffPutGet+++++"
  putFirstDiffPutGet
  putStrLn "\n+++++putFirstDiffPutDet+++++"
  putFirstDiffPutDet
  putStrLn "\n+++++putFirstDiffPutStab+++++"
  putFirstDiffPutStab
  putStrLn "\n+++++cityGetPut+++++"
  cityGetPut
  putStrLn "\n+++++cityPutGet+++++"
  cityPutGet
  putStrLn "\n+++++cityPutDet+++++"
  cityPutDet
  putStrLn "\n+++++cityPutStab+++++"
  cityPutStab
  putStrLn "\n+++++nameGetPut+++++"
  nameGetPut
  putStrLn "\n+++++namePutGet+++++"
  namePutGet
  putStrLn "\n+++++namePutDet+++++"
  namePutDet
  putStrLn "\n+++++namePutStab+++++"
  namePutStab
  putStrLn "\n+++++putAsGetPut+++++"
  putAsGetPut
  putStrLn "\n+++++putAsPutGet+++++"
  putAsPutGet
  putStrLn "\n+++++putAsPutDet+++++"
  putAsPutDet
  putStrLn "\n+++++putAsPutStab+++++"
  putAsPutStab
  putStrLn "\n+++++putStudentsGetPut+++++"
  putStudentsGetPut
  putStrLn "\n+++++putStudentsPutGet+++++"
  putStudentsPutGet
  putStrLn "\n+++++putStudentsPutDet+++++"
  putStudentsPutDet
  putStrLn "\n+++++putStudentsPutStab+++++"
  putStudentsPutStab
  putStrLn "\n+++++putHeadGetPut+++++"
  putHeadGetPut
  putStrLn "\n+++++putHeadPutGet+++++"
  putHeadPutGet
  putStrLn "\n+++++putHeadPutDet+++++"
  putHeadPutDet
  putStrLn "\n+++++putHeadPutStab+++++"
  putHeadPutStab
  putStrLn "\n+++++putTailGetPut+++++"
  putTailGetPut
  putStrLn "\n+++++putTailPutGet+++++"
  putTailPutGet
  putStrLn "\n+++++putTailPutDet+++++"
  putTailPutDet
  putStrLn "\n+++++putTailPutStab+++++"
  putTailPutStab
  putStrLn "\n+++++putTakeGetPut+++++"
  putTakeGetPut
  putStrLn "\n+++++putTakePutGet+++++"
  putTakePutGet
  putStrLn "\n+++++putTakePutDet+++++"
  putTakePutDet
  putStrLn "\n+++++putTakePutStab+++++"
  putTakePutStab
  putStrLn "\n+++++putZipGetPut+++++"
  putZipGetPut
  putStrLn "\n+++++putZipPutGet+++++"
  putZipPutGet
  putStrLn "\n+++++putZipPutDet+++++"
  putZipPutDet
  putStrLn "\n+++++putZipPutStab+++++"
  putZipPutStab
  putStrLn "\n+++++putZip'GetPut+++++"
  putZip'GetPut
  putStrLn "\n+++++putZip'PutGet+++++"
  putZip'PutGet
  putStrLn "\n+++++putZip'PutDet+++++"
  putZip'PutDet
  putStrLn "\n+++++putZip'PutStab+++++"
  putZip'PutStab
  putStrLn "\n+++++putAltGetPut+++++"
  putAltGetPut
  putStrLn "\n+++++putAltPutGet+++++"
  putAltPutGet
  putStrLn "\n+++++putAltPutDet+++++"
  putAltPutDet
  putStrLn "\n+++++putAltPutStab+++++"
  putAltPutStab
  putStrLn "\n+++++putAppendGetPut+++++"
  putAppendGetPut
  putStrLn "\n+++++putAppendPutGet+++++"
  putAppendPutGet
  putStrLn "\n+++++putAppendPutDet+++++"
  putAppendPutDet
  putStrLn "\n+++++putAppendPutStab+++++"
  putAppendPutStab
  putStrLn "\n+++++putInitGetPut+++++"
  putInitGetPut
  putStrLn "\n+++++putInitPutGet+++++"
  putInitPutGet
  putStrLn "\n+++++putInitPutDet+++++"
  putInitPutDet
  putStrLn "\n+++++putInitPutStab+++++"
  putInitPutStab
  putStrLn "\n+++++putConsGetPut+++++"
  putConsGetPut
  putStrLn "\n+++++putConsPutGet+++++"
  putConsPutGet
  putStrLn "\n+++++putConsPutDet+++++"
  putConsPutDet
  putStrLn "\n+++++putConsPutStab+++++"
  putConsPutStab
  putStrLn "\n+++++putUnconsGetPut+++++"
  putUnconsGetPut
  putStrLn "\n+++++putUnconsPutGet+++++"
  putUnconsPutGet
  putStrLn "\n+++++putUnconsPutDet+++++"
  putUnconsPutDet
  putStrLn "\n+++++putUnconsPutStab+++++"
  putUnconsPutStab
  putStrLn "\n+++++filterLPutGetPut+++++"
  filterLPutGetPut
  putStrLn "\n+++++filterLPutPutGet+++++"
  filterLPutPutGet
  putStrLn "\n+++++filterLPutPutDet+++++"
  filterLPutPutDet
  putStrLn "\n+++++filterLPutPutStab+++++"
  filterLPutPutStab
  putStrLn "\n+++++putSumTreeShiftGetPut+++++"
  putSumTreeShiftGetPut
  putStrLn "\n+++++putSumTreeShiftPutGet+++++"
  putSumTreeShiftPutGet
  putStrLn "\n+++++putSumTreeShiftPutDet+++++"
  putSumTreeShiftPutDet
  putStrLn "\n+++++putSumTreeShiftPutStab+++++"
  putSumTreeShiftPutStab
  putStrLn "\n+++++putSumTreeDivNatGetPut+++++"
  putSumTreeDivNatGetPut
  putStrLn "\n+++++putSumTreeDivNatPutGet+++++"
  putSumTreeDivNatPutGet
  putStrLn "\n+++++putSumTreeDivNatPutDet+++++"
  putSumTreeDivNatPutDet
  putStrLn "\n+++++putSumTreeDivNatPutStab+++++"
  putSumTreeDivNatPutStab
