!##################################################################################################
!                                               CEOS
!
! - Plane Strain, Axisymmetric and 3D Analysis.
! - Nonlinear Geometric Analysis (Current Lagrangian Formulation).
! - Nonlinear Constitutive Material Module.
! - Parallel Direct Sparse Solver - PARDISO
! - Full Newton-Raphson Procedure
! - GiD Interface (Pre and Post Processing)
!
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Remarks:
!##################################################################################################
program MAIN


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! ---------------------------------------------------------------------------------------------
    use ModFEMAnalysis
    use ModFEMAnalysisBiphasic
    use ModProbe
    use ModPostProcessors
    use ModExportResultFile
    use ModTools
    use Timer
    use Parser
    use ModAnalysisManager
    use ModAnalysis

    implicit none

    ! Objects
	! ---------------------------------------------------------------------------------------------
    class (ClassFEMAnalysis), pointer :: Analysis
    type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
    class(ClassPostProcessor), pointer :: PostProcessor


    ! Internal variables
	! ---------------------------------------------------------------------------------------------

    character(len=100), allocatable, dimension(:) :: Args
    type(ClassTimer)                              :: AnalysisTime
    type(ClassParser)                             :: Comp

    character(len=255)                            :: SettingsFileName , PostProcessingFileName
    Logical                                       :: TaskSolve , TaskPostProcess
   
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

! TODO (Thiago#1#11/17/15): Trocar todos o nome dos m�dulos para Mod'NOME'


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	!                                       MAIN PROGRAM
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	write(*,*) '---------------------------------------------------------'
    write(*,*) '                         CEOS'
    write(*,*) '---------------------------------------------------------'

    
    !**********************************************************************************************
    ! Reading Arguments
    !**********************************************************************************************
    call ArgumentHandler(TaskSolve , TaskPostProcess ,SettingsFileName , PostProcessingFileName)
    !**********************************************************************************************
    write(*,*) ''
    write(*,*) 'Settings File Name: '//trim(SettingsFileName)
    write(*,*) ''
    if (TaskSolve) then
        write(*,*) 'Problem will be solved'
    else
        write(*,*) 'Problem will *NOT* be solved'
    endif
    write(*,*) ''
    if (TaskPostProcess) then
        write(*,*) 'Problem will be postprocessed'
        write(*,*) 'PostProcessing File Name: '//trim(PostProcessingFileName)
    else
        write(*,*) 'Problem will *NOT* be postprocessed'
    endif
    write(*,*) ''


    ! Reading settings file and Create Analysis (FEM or Multiscale)
    ! ---------------------------------------------------------------------------------------------
	call ReadAndCreateAnalysis(Analysis, SettingsFileName)


	if (TaskSolve) then
        !**********************************************************************************************
        ! SOLVING A FINITE ELEMENT ANALYSIS
        !**********************************************************************************************

        write(*,*) '---------------------------------------------------------'
        write(*,*) 'SOLVING'
        write(*,*) '---------------------------------------------------------'

        ! Solve FEM Analysis
        ! ---------------------------------------------------------------------------------------------
        call AnalysisTime%Start

        ! Allocating memory for the sparse matrix (pre-assembling)
        ! ---------------------------------------------------------------------------------------------
        !call Analysis%AllocateGlobalSparseStiffnessMatrix
        call Analysis%AllocateKgSparseUpperTriangular

        call Analysis%Solve

        call AnalysisTime%Stop
        write(*,*) ''
        write(*,*) ''
        write(*,*) 'Finite Element Analysis: CPU Time =', AnalysisTime%GetElapsedTime() , '[s]'
        write(*,*) ''
        write(*,*) ''
        !**********************************************************************************************
    endif

    if (TaskPostProcess) then
    !**********************************************************************************************
    ! POSTPROCESSING THE FINITE ELEMENT ANALYSIS RESULTS
    !**********************************************************************************************

        call AnalysisTime%Start
        write(*,*) '---------------------------------------------------------'
        write(*,*) 'POST PROCESSING'
        write(*,*) '---------------------------------------------------------'
        write(*,*) ''

        ! Reading Probes Input File
        ! ---------------------------------------------------------------------------------------------
        call ReadPostProcessingInputFile(PostProcessingFileName,ProbeList,PostProcessor)
        write(*,*) ''

        ! Post Processing Results
        ! ---------------------------------------------------------------------------------------------
        !call PostProcessingResults(ProbeList,PostProcessor,Analysis)
        
        
        select case (Analysis%AnalysisSettings%ProblemType)
       
            case (ProblemTypes%Mechanical)
                ! Post Processing Results
                ! ---------------------------------------------------------------------------------------------
                call PostProcessingResults(ProbeList,PostProcessor,Analysis)
                
            case (ProblemTypes%Thermal)
                stop ('ERROR: Thermal analysis not implemented')
                
            case (ProblemTypes%Biphasic)
                ! Post Processing Results
                ! ---------------------------------------------------------------------------------------------
                call PostProcessingResultsBiphasic(ProbeList,PostProcessor,Analysis)
        
        end select
        
        
                
   

        call AnalysisTime%Stop
        write(*,*) ''
        write(*,*) ''
        write(*,*) 'CPU Time =', AnalysisTime%GetElapsedTime() , '[s]'
        write(*,*) '---------------------------------------------------------'
        write(*,*) ''
        write(*,*) ''
        !**********************************************************************************************
    endif


    ! TODO (Thiago#1#11/03/15): Padronizar gerenciamento de erros.



	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end program MAIN
!##################################################################################################
