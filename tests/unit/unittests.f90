!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : unittest
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> It runs all tests 
!------------------------------------------------------------------------------

program unittest

  use datetime_test, only: testSuiteDatetime
  use tst_test, only: testSuiteTst
  use strings_test, only: testSuiteStrings
  use dict_test, only: testSuiteDict
  use orderedDict_test, only: testSuiteOrderedDict
  use arrayUtils_test, only: testSuiteArrayUtils
  use perfTimer_test, only: testSuitePerfTimer
  use file_test, only: testSuiteFile
  use configParser_test, only: testSuiteConfigParser
  use configFile_test, only: testSuiteConfigFile
  use typeOptions_test, only: testSuiteTypeOptions
  use cfgOptions_test, only: testSuiteCfgOptions
  use oopExtends_test, only: testSuiteOopExtends
  use linkedList_test, only: testSuiteLinkedList
  use set_test, only: testSuiteSet
  use shr_mask_test, only: testSuiteMask
  use shr_maskIndices_test, only: testSuiteMaskIndices
  use shr_maskClusters_test, only: testSuiteMaskClusters

  ! array
  use shr_arrayRealDim_test, only: testSuiteArrayRealDim
  use arrayRsp_test, only: testSuiteArrayRsp
  use shr_arrayGridFullRsp_test, only: testSuiteArrayGridFullRsp
  use shr_arrayContainerAllocatable_test, only: testSuiteArrayContainerAllocatable

  ! grid related
  use shr_grid_test, only: testSuiteGrid
  use shr_gridPartition_test, only: testSuiteGridPartition
  use shr_gridcell_test, only: testSuiteGridCell
  use shr_coord_test, only: testSuiteCoord
  use shr_gAxis_test, only: testSuitegAxis
  use shr_gAxisBounds_test, only: testSuitegAxisBounds
  use shr_gGridAxesCell_test, only: testSuitegGridAxesCell
  use shr_gAxisMapping_test, only: testSuitegAxisMapping
  use shr_gAxisCellIndex_test, only: testSuitegAxisCellIndex
  use shr_gridMask_test, only: testSuiteGridMask
  use shr_gGridArrayMap_test, only: testSuitegGridArrayMap
  use shr_gGridCellsMap_test, only: testSuitegGridCellsMap
  use shr_gridMaskBorder_test, only: testSuitegridMaskBorder
  use shr_gridMaskClusters_test, only: testSuitegridMaskClusters
  use shr_gridMaskClustersIterator_test, only: testSuitegridMaskClustersIterator
  use shr_gGridDescriptor_test, only: testSuitegGridDescriptor
  use shr_gridBounds_test, only: testSuiteGridBounds
  use shr_gridDomainSquared_test, only: testSuitegridDomainSquared
  use shr_gridDomainToSquaredConverter_test, only: testSuitegridDomainToSquaredConverter
  use shr_gridDomain_test, only: testSuitegridDomain

  implicit none

  ! declare each test
  type(testSuiteDatetime) :: tsDatetime
  type(testSuiteTst) :: tsTst
  type(testSuiteStrings) :: tsStrings
  type(testSuiteDict) :: tsDict
  type(testSuiteOrderedDict) :: tsOrderedDict
  type(testSuiteArrayUtils) :: tsArrayUtils
  type(testSuitePerfTimer) :: tsPerfTimer
  type(testSuiteFile) :: tsFile
  type(testSuiteConfigParser) :: tsConfigParser
  type(testSuiteConfigFile) :: tsConfigFile
  type(testSuiteTypeOptions) :: tsTypeOptions
  type(testSuiteCfgOptions) :: tsCfgOptions
  type(testSuiteOopExtends) :: tsOOPExtends
  type(testSuiteLinkedList) :: tsLinkedList
  type(testSuiteSet) :: tsSet
  type(testSuiteMask) :: tsMask
  type(testSuiteMaskIndices) :: tsMaskIndices
  type(testSuiteMaskClusters) :: tsMaskClusters

  type(testSuiteArrayRealDim) :: tsArrayRealDim
  type(testSuiteArrayRsp) :: tsArrayRsp
  type(testSuiteArrayGridFullRsp) :: tsArrayGridFullRsp
  type(testSuiteArrayContainerAllocatable) :: tsArrayContainerAllocatable
  type(testSuiteGrid) :: tsGrid
  type(testSuiteGridPartition) :: tsGridPartition
  type(testSuiteGridCell) :: tsGridcell
  type(testSuiteCoord) :: tsCoord
  type(testSuitegAxisBounds) :: tsgAxisBounds
  type(testSuitegAxis) :: tsgAxis
  type(testSuitegGridAxesCell) :: tsgGridAxesCell
  type(testSuitegAxisMapping) :: tsgAxisMapping
  type(testSuitegAxisCellIndex) :: tsgAxisCellIndex
  type(testSuiteGridMask) :: tsGridMask
  type(testSuitegGridArrayMap) :: tsgGridArrayMap
  type(testSuitegGridCellsMap) :: tsgGridCellMap
  type(testSuitegridMaskBorder) :: tsGridMaskBorder
  type(testSuitegridMaskClustersIterator) :: tsgridMaskClustersIterator
  type(testSuitegridMaskClusters) :: tsGridMaskClusters
  type(testSuitegGridDescriptor) :: tsGridDescriptor
  type(testSuiteGridBounds) :: tsGridBounds
  type(testSuitegridDomainSquared) :: tsGridDomainSquared
  type(testSuitegridDomainToSquaredConverter) :: tsGridDomainToSquaredConverter
  type(testSuitegridDomain) :: tsGridDomain

  ! declare tests
  call tsDatetime % init("Datetime test", 330)
  call tsTst % init("Ternary search tree test", 30)
  call tsStrings % init("Strings test", 70)
  call tsDict % init("Dict test", 10)
  call tsOrderedDict % init("Ordered dict test", 10)
  call tsArrayUtils % init("Array utils test", 20)
  call tsPerfTimer % init("Performance timer test", 20)
  call tsFile % init("File test", 10)
  call tsOOPExtends % init("OOP Extends test", 10)
  call tsConfigParser % init("(CFG) Config parser test", 30)
  call tsConfigFile % init("(CFG) Config file test", 20)
  call tsTypeOptions % init("(CFG) Type options test", 40)
  call tsCfgOptions % init("(CFG) Cfg options test", 20)
  call tsLinkedList % init("Linked list test", 10)
  call tsSet % init("Set test", 10)
  call tsMask % init("Mask test", 20)
  call tsMaskIndices % init("MaskIndices test", 20)
  call tsMaskClusters % init("Mask Clusters test", 30)

  call tsArrayRealDim % init("ArrayRealDim test", 30)
  call tsArrayRsp % init("ArrayRsp test", 20)
  call tsArrayGridFullRsp % init("ArrayGridFullRsp test", 20)
  call tsArrayContainerAllocatable % init("ArrayContainerAllocatable test", 20)

  call tsGrid % init("Grid test", 40)
  call tsGridPartition % init("Grid partition test", 20)
  call tsGridcell % init("Gridcell test", 20)
  call tsCoord % init("Coordinate test", 10)
  call tsgAxisBounds % init("gAxisBounds test", 20)
  call tsgGridAxesCell % init("gGridAxesCell test", 20)
  call tsgAxis % init("gAxis test", 20)
  call tsgAxisMapping % init("gAxisMapping test", 20)
  call tsgAxisCellIndex % init("gAxisCellIndex test", 20)
  call tsGridMask % init("gridMask test", 20)
  call tsgGridArrayMap % init("gridArrayMap test", 20)
  call tsgGridCellMap % init("gridcellsMap test", 20)
  call tsGridMaskBorder % init("gridMaskBorder test", 20)
  call tsGridMaskClusters % init("gridMaskClusters test", 20)
  call tsgridMaskClustersIterator % init("gridMaskClustersIterator test", 10)
  call tsGridDescriptor % init("gridDescriptor test", 20)
  call tsGridBounds % init("grid bounds test", 30)
  call tsGridDomainSquared % init("grid domain squared test", 10)
  call tsGridDomainToSquaredConverter % init("grid domain to squared converter test", 10)
  call tsGridDomain % init("grid domain test", 10)
  

  ! run all tests 
  call tsDatetime % run()
  call tsTst % run() 
  call tsStrings % run() 
  call tsDict % run() 
  call tsOrderedDict % run() 
  call tsArrayUtils % run() 
  call tsPerfTimer % run() 
  call tsFile % run() 
  call tsConfigParser % run() 
  call tsConfigFile % run() 
  call tsTypeOptions % run() 
  call tsCfgOptions % run() 
  call tsOOPExtends % run() 
  call tsLinkedList % run() 
  call tsSet % run()
  call tsMask % run()
  call tsMaskIndices % run()
  call tsMaskClusters % run()

  call tsArrayRealDim % run() 
  call tsArrayContainerAllocatable % run() 
  call tsArrayRsp % run() 
  call tsArrayGridFullRsp % run() 
  call tsGrid % run() 
  call tsGridPartition % run() 
  call tsGridCell % run() 
  call tsCoord % run() 
  call tsgAxisBounds % run()
  call tsgGridAxesCell % run() 
  call tsgAxis % run()
  call tsgAxisMapping % run()
  call tsgAxisCellIndex % run()
  call tsGridMask % run()
  call tsgGridArrayMap % run()
  call tsgGridCellMap % run()
  call tsGridMaskBorder % run()
  call tsGridMaskClusters % run()
  call tsgridMaskClustersIterator % run()
  call tsGridDescriptor % run()
  call tsGridBounds % run()
  call tsGridDomainSquared % run()
  call tsGridDomainToSquaredConverter % run()
  call tsGridDomain % run()


  ! show results
  call tsDatetime % report()
  call tsTst % report() 
  call tsStrings % report() 
  call tsDict % report() 
  call tsOrderedDict % report() 
  call tsArrayUtils % report() 
  call tsPerfTimer % report() 
  call tsFile % report()
  call tsConfigParser % report() 
  call tsConfigFile % report() 
  call tsTypeOptions % report() 
  call tsCfgOptions % report() 
  call tsOOPExtends % report()
  call tsLinkedList % report() 
  call tsSet % report()
  call tsMask % report()
  call tsMaskIndices % report()
  call tsMaskClusters % report()

  call tsArrayRealDim % report() 
  call tsArrayContainerAllocatable % report() 
  call tsArrayRsp % report() 
  call tsArrayGridFullRsp % report() 
  call tsGrid % report() 
  call tsGridPartition % report() 
  call tsGridcell % report() 
  call tsCoord % report() 
  call tsgAxisBounds % report()
  call tsgGridAxesCell % report()
  call tsgAxis % report()
  call tsgAxisMapping % report()
  call tsgAxisCellIndex % report()
  call tsGridMask % report()
  call tsgGridArrayMap % report()
  call tsgGridCellMap % report()
  call tsGridMaskBorder % report()
  call tsGridMaskClusters % report()
  call tsgridMaskClustersIterator % report()
  call tsGridDescriptor % report()
  call tsGridBounds % report()
  call tsGridDomainSquared % report()
  call tsGridDomainToSquaredConverter % report()
  call tsGridDomain % report()

  ! crash if any of the tests fail
  if (.not. tsDatetime % isSuccessful() .or. &
      .not. tsTst % isSuccessful() .or. &
      .not. tsStrings % isSuccessful() .or. & 
      .not. tsDict % isSuccessful() .or. &
      .not. tsOrderedDict % isSuccessful() .or. &
      .not. tsArrayUtils % isSuccessful() .or. &
      .not. tsPerfTimer % isSuccessful() .or. &
      .not. tsFile % isSuccessful() .or. &
      .not. tsConfigParser % isSuccessful() .or. & 
      .not. tsConfigFile % isSuccessful() .or. & 
      .not. tsTypeOptions % isSuccessful() .or. & 
      .not. tsCfgOptions % isSuccessful() .or. & 
      .not. tsOOPExtends % isSuccessful() .or. &
      .not. tsLinkedList % isSuccessful() .or. &
      .not. tsSet % isSuccessful() .or. &
      .not. tsMask % isSuccessful() .or. &
      .not. tsMaskIndices % isSuccessful() .or. &
      .not. tsMaskClusters % isSuccessful() .or. &
      .not. tsArrayRealDim % isSuccessful() .or. &
      .not. tsArrayContainerAllocatable % isSuccessful() .or. &
      .not. tsArrayRsp % isSuccessful() .or. &
      .not. tsArrayGridFullRsp % isSuccessful() .or. &
      .not. tsGrid % isSuccessful() .or. &
      .not. tsGridPartition % isSuccessful() .or. &
      .not. tsGridcell % isSuccessful() .or. &
      .not. tsCoord % isSuccessful() .or. &
      .not. tsgAxisBounds % isSuccessful() .or. &
      .not. tsgGridAxesCell % isSuccessful() .or. &
      .not. tsgAxis % isSuccessful() .or. &
      .not. tsgAxisMapping % isSuccessful() .or. & 
      .not. tsgAxisCellIndex % isSuccessful() .or. &
      .not. tsGridMask % isSuccessful() .or. &
      .not. tsgGridArrayMap % isSuccessful() .or. &
      .not. tsgGridCellMap % isSuccessful() .or. &
      .not. tsGridMaskBorder % isSuccessful() .or. &
      .not. tsGridMaskClusters % isSuccessful() .or. &
      .not. tsgridMaskClustersIterator % isSuccessful() .or. &
      .not. tsGridDescriptor % isSuccessful() .or. &
      .not. tsGridBounds % isSuccessful() .or. &
      .not. tsGridDomainSquared % isSuccessful() .or. &
      .not. tsGridDomainToSquaredConverter % isSuccessful() .or. &
      .not. tsGridDomain % isSuccessful() &
      ) stop 1

end program unittest
