module TestLensExamples where

import BinaryList
import LensCheck
import LensExamples
import EasyCheck

dateLensGetPut :: IO ()
dateLensGetPut = easyCheck2 (checkGetPut dateLens)

dateLensPutGet :: IO ()
dateLensPutGet = easyCheck1 (checkPutGet dateLens)

dateLensPutPut :: IO ()
dateLensPutPut = easyCheck3 (checkPutPut dateLens)

monthLensGetPut :: IO ()
monthLensGetPut = easyCheck2 (checkGetPut monthLens)

monthLensPutGet :: IO ()
monthLensPutGet = easyCheck1 (checkPutGet monthLens)

monthLensPutPut :: IO ()
monthLensPutPut = easyCheck3 (checkPutPut monthLens)

dayLensGetPut :: IO ()
dayLensGetPut = easyCheck2 (checkGetPut dayLens)

dayLensPutGet :: IO ()
dayLensPutGet = easyCheck1 (checkPutGet dayLens)

dayLensPutPut :: IO ()
dayLensPutPut = easyCheck3 (checkPutPut dayLens)

addressLensGetPut :: IO ()
addressLensGetPut = easyCheck2 (checkGetPut addressLens)

addressLensPutGet :: IO ()
addressLensPutGet = easyCheck1 (checkPutGet addressLens)

addressLensPutPut :: IO ()
addressLensPutPut = easyCheck3 (checkPutPut addressLens)

nameLensGetPut :: IO ()
nameLensGetPut = easyCheck2 (checkGetPut nameLens)

nameLensPutGet :: IO ()
nameLensPutGet = easyCheck1 (checkPutGet nameLens)

nameLensPutPut :: IO ()
nameLensPutPut = easyCheck3 (checkPutPut nameLens)

falsePutGetPut :: IO ()
falsePutGetPut = easyCheck2 (checkGetPut falsePut)

falsePutPutGet :: IO ()
falsePutPutGet = easyCheck1 (checkPutGet falsePut)

falsePutPutPut :: IO ()
falsePutPutPut = easyCheck3 (checkPutPut falsePut)

putHoursGetPut :: IO ()
putHoursGetPut = easyCheck2 (checkGetPut putHours)

putHoursPutGet :: IO ()
putHoursPutGet = easyCheck1 (checkPutGet putHours)

putHoursPutPut :: IO ()
putHoursPutPut = easyCheck3 (checkPutPut putHours)

putMinsGetPut :: IO ()
putMinsGetPut = easyCheck2 (checkGetPut putMins)

putMinsPutGet :: IO ()
putMinsPutGet = easyCheck1 (checkPutGet putMins)

putMinsPutPut :: IO ()
putMinsPutPut = easyCheck3 (checkPutPut putMins)

putHalveBinaryListGetPut :: IO ()
putHalveBinaryListGetPut = easyCheck2 (checkGetPut putHalveBinaryList)

putHalveBinaryListPutGet :: IO ()
putHalveBinaryListPutGet = easyCheck1 (checkPutGet putHalveBinaryList)

putHalveBinaryListPutPut :: IO ()
putHalveBinaryListPutPut = easyCheck3 (checkPutPut putHalveBinaryList)

putHalvePeanoGetPut :: IO ()
putHalvePeanoGetPut = easyCheck1 (checkListGetPut putHalvePeano)

putHalvePeanoPutGet :: IO ()
putHalvePeanoPutGet = easyCheck (checkListPutGet putHalvePeano)

putHalvePeanoPutPut :: IO ()
putHalvePeanoPutPut = easyCheck2 (checkListPutPut putHalvePeano)

putflattenGetPut :: IO ()
putflattenGetPut = easyCheck2 (checkGetPut putflatten)

putflattenPutGet :: IO ()
putflattenPutGet = easyCheck1 (checkPutGet putflatten)

putflattenPutPut :: IO ()
putflattenPutPut = easyCheck3 (checkPutPut putflatten)

putFirstGetPut :: IO ()
putFirstGetPut = easyCheck2 (checkGetPut putFirst)

putFirstPutGet :: IO ()
putFirstPutGet = easyCheck1 (checkPutGet putFirst)

putFirstPutPut :: IO ()
putFirstPutPut = easyCheck3 (checkPutPut putFirst)

putSecondGetPut :: IO ()
putSecondGetPut = easyCheck2 (checkGetPut putSecond)

putSecondPutGet :: IO ()
putSecondPutGet = easyCheck1 (checkPutGet putSecond)

putSecondPutPut :: IO ()
putSecondPutPut = easyCheck3 (checkPutPut putSecond)

putFirstCountGetPut :: IO ()
putFirstCountGetPut = easyCheck2 (checkGetPut putFirstCount)

putFirstCountPutGet :: IO ()
putFirstCountPutGet = easyCheck1 (checkPutGet putFirstCount)

putFirstCountPutPut :: IO ()
putFirstCountPutPut = easyCheck3 (checkPutPut putFirstCount)

putFirstDiffGetPut :: IO ()
putFirstDiffGetPut = easyCheck2 (checkGetPut putFirstDiff)

putFirstDiffPutGet :: IO ()
putFirstDiffPutGet = easyCheck1 (checkPutGet putFirstDiff)

putFirstDiffPutPut :: IO ()
putFirstDiffPutPut = easyCheck3 (checkPutPut putFirstDiff)

cityGetPut :: IO ()
cityGetPut = easyCheck2 (checkGetPut city)

cityPutGet :: IO ()
cityPutGet = easyCheck1 (checkPutGet city)

cityPutPut :: IO ()
cityPutPut = easyCheck3 (checkPutPut city)

nameGetPut :: IO ()
nameGetPut = easyCheck2 (checkGetPut name)

namePutGet :: IO ()
namePutGet = easyCheck1 (checkPutGet name)

namePutPut :: IO ()
namePutPut = easyCheck3 (checkPutPut name)

mergePeopleGetPut :: IO ()
mergePeopleGetPut = easyCheck1 (checkListGetPut mergePeople)

mergePeoplePutGet :: IO ()
mergePeoplePutGet = easyCheck (checkListPutGet mergePeople)

mergePeoplePutPut :: IO ()
mergePeoplePutPut = easyCheck2 (checkListPutPut mergePeople)

putAsGetPut :: IO ()
putAsGetPut = easyCheck1 (checkListGetPut putAs)

putAsPutGet :: IO ()
putAsPutGet = easyCheck (checkListPutGet putAs)

putAsPutPut :: IO ()
putAsPutPut = easyCheck2 (checkListPutPut putAs)

putStudentsGetPut :: IO ()
putStudentsGetPut = easyCheck1 (checkListGetPut putStudents)

putStudentsPutGet :: IO ()
putStudentsPutGet = easyCheck (checkListPutGet putStudents)

putStudentsPutPut :: IO ()
putStudentsPutPut = easyCheck2 (checkListPutPut putStudents)

putHeadGetPut :: IO ()
putHeadGetPut = easyCheck1 (checkListGetPut putHead)

putHeadPutGet :: IO ()
putHeadPutGet = easyCheck (checkListPutGet putHead)

putHeadPutPut :: IO ()
putHeadPutPut = easyCheck2 (checkListPutPut putHead)

putTailGetPut :: IO ()
putTailGetPut = easyCheck1 (checkListGetPut putTail)

putTailPutGet :: IO ()
putTailPutGet = easyCheck (checkListPutGet putTail)

putTailPutPut :: IO ()
putTailPutPut = easyCheck2 (checkListPutPut putTail)

putReverseGetPut :: IO ()
putReverseGetPut = easyCheck1 (checkListGetPut putReverse)

putReversePutGet :: IO ()
putReversePutGet = easyCheck (checkListPutGet putReverse)

putReversePutPut :: IO ()
putReversePutPut = easyCheck2 (checkListPutPut putReverse)

putLengthGetPut :: IO ()
putLengthGetPut = easyCheck1 (checkListGetPut putLength)

putLengthPutGet :: IO ()
putLengthPutGet = easyCheck (checkListPutGet putLength)

putLengthPutPut :: IO ()
putLengthPutPut = easyCheck2 (checkListPutPut putLength)

putZipGetPut :: IO ()
putZipGetPut = easyCheck2 (checkGetPut putZip)

putZipPutGet :: IO ()
putZipPutGet = easyCheck1 (checkPutGet putZip)

putZipPutPut :: IO ()
putZipPutPut = easyCheck3 (checkPutPut putZip)

putZip'GetPut :: IO ()
putZip'GetPut = easyCheck2 (checkGetPut putZip')

putZip'PutGet :: IO ()
putZip'PutGet = easyCheck1 (checkPutGet putZip')

putZip'PutPut :: IO ()
putZip'PutPut = easyCheck3 (checkPutPut putZip')

putAltGetPut :: IO ()
putAltGetPut = easyCheck1 (checkListGetPut putAlt)

putAltPutGet :: IO ()
putAltPutGet = easyCheck (checkListPutGet putAlt)

putAltPutPut :: IO ()
putAltPutPut = easyCheck2 (checkListPutPut putAlt)

putAppendGetPut :: IO ()
putAppendGetPut = easyCheck2 (checkGetPut putAppend)

putAppendPutGet :: IO ()
putAppendPutGet = easyCheck1 (checkPutGet putAppend)

putAppendPutPut :: IO ()
putAppendPutPut = easyCheck3 (checkPutPut putAppend)

putDropGetPut :: IO ()
putDropGetPut = easyCheck1 (checkListGetPut putDrop)

putDropPutGet :: IO ()
putDropPutGet = easyCheck (checkListPutGet putDrop)

putDropPutPut :: IO ()
putDropPutPut = easyCheck2 (checkListPutPut putDrop)

putInitGetPut :: IO ()
putInitGetPut = easyCheck1 (checkListGetPut putInit)

putInitPutGet :: IO ()
putInitPutGet = easyCheck (checkListPutGet putInit)

putInitPutPut :: IO ()
putInitPutPut = easyCheck2 (checkListPutPut putInit)

putConsGetPut :: IO ()
putConsGetPut = easyCheck1 (checkListGetPut putCons)

putConsPutGet :: IO ()
putConsPutGet = easyCheck (checkListPutGet putCons)

putConsPutPut :: IO ()
putConsPutPut = easyCheck2 (checkListPutPut putCons)

putUnconsGetPut :: IO ()
putUnconsGetPut = easyCheck2 (checkGetPut putUncons)

putUnconsPutGet :: IO ()
putUnconsPutGet = easyCheck1 (checkPutGet putUncons)

putUnconsPutPut :: IO ()
putUnconsPutPut = easyCheck3 (checkPutPut putUncons)

filterLPutGetPut :: IO ()
filterLPutGetPut = easyCheck1 (checkListGetPut filterLPut)

filterLPutPutGet :: IO ()
filterLPutPutGet = easyCheck (checkListPutGet filterLPut)

filterLPutPutPut :: IO ()
filterLPutPutPut = easyCheck2 (checkListPutPut filterLPut)

putSumTreeGetPut :: IO ()
putSumTreeGetPut = easyCheck2 (checkGetPut putSumTree)

putSumTreePutGet :: IO ()
putSumTreePutGet = easyCheck1 (checkPutGet putSumTree)

putSumTreePutPut :: IO ()
putSumTreePutPut = easyCheck3 (checkPutPut putSumTree)

putSumTreeShiftGetPut :: IO ()
putSumTreeShiftGetPut = easyCheck2 (checkGetPut putSumTreeShift)

putSumTreeShiftPutGet :: IO ()
putSumTreeShiftPutGet = easyCheck1 (checkPutGet putSumTreeShift)

putSumTreeShiftPutPut :: IO ()
putSumTreeShiftPutPut = easyCheck3 (checkPutPut putSumTreeShift)

putSumTreeDivNatGetPut :: IO ()
putSumTreeDivNatGetPut = easyCheck2 (checkGetPut putSumTreeDivNat)

putSumTreeDivNatPutGet :: IO ()
putSumTreeDivNatPutGet = easyCheck1 (checkPutGet putSumTreeDivNat)

putSumTreeDivNatPutPut :: IO ()
putSumTreeDivNatPutPut = easyCheck3 (checkPutPut putSumTreeDivNat)

putLinesGetPut :: IO ()
putLinesGetPut = easyCheck1 (checkListGetPut putLines)

putLinesPutGet :: IO ()
putLinesPutGet = easyCheck (checkListPutGet putLines)

putLinesPutPut :: IO ()
putLinesPutPut = easyCheck2 (checkListPutPut putLines)

mainTest = do
  dateLensGetPut
  dateLensPutGet
  monthLensGetPut
  monthLensPutGet
  dayLensGetPut
  dayLensPutGet
  addressLensGetPut
  addressLensPutGet
  nameLensGetPut
  nameLensPutGet
  -- falsePutGetPut
  -- falsePutPutGet
  putHoursGetPut
  putHoursPutGet
  putMinsGetPut
  putMinsPutGet
  putHalvePeanoGetPut
  putHalvePeanoPutGet
  putflattenGetPut
  putflattenPutGet
  putFirstGetPut
  putFirstPutGet
  putSecondGetPut
  putSecondPutGet
  putFirstCountGetPut
  putFirstCountPutGet
  putFirstDiffGetPut
  putFirstDiffPutGet
  cityGetPut
  cityPutGet
  nameGetPut
  namePutGet
  mergePeopleGetPut
  mergePeoplePutGet
  putAsGetPut
  putAsPutGet
  putStudentsGetPut
  putStudentsPutGet
  putHeadGetPut
  putHeadPutGet
  putTailGetPut
  putTailPutGet
  putReverseGetPut
  putReversePutGet
  putLengthGetPut
  putLengthPutGet
  putZipGetPut
  putZipPutGet
  putZip'GetPut
  putZip'PutGet
  putAltGetPut
  putAltPutGet
  putAppendGetPut
  putAppendPutGet
  putDropGetPut
  putDropPutGet
  putInitGetPut
  putInitPutGet
  putConsGetPut
  putConsPutGet
  putUnconsGetPut
  putUnconsPutGet
  filterLPutGetPut
  filterLPutPutGet
  putSumTreeGetPut
  putSumTreePutGet
  putSumTreeShiftGetPut
  putSumTreeShiftPutGet
  putSumTreeDivNatGetPut
  putSumTreeDivNatPutGet
  putLinesGetPut
  putLinesPutGet
