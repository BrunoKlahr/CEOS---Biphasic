!##################################################################################################
! This module has the attributes and methods to select the parameters of the analysis type chosen.
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
module ModAnalysisManager

    use ModMultiscaleAnalysis
    use ModReadInputFile
    use ModAnalysis
    use ModFEMAnalysisBiphasic

    contains


    subroutine ReadAndCreateAnalysis(Analysis, FileName)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        implicit none

        ! Object
        ! -----------------------------------------------------------------------------------
        class(ClassFEMAnalysis), pointer :: Analysis

        ! Input variables
        ! -----------------------------------------------------------------------------------
        type (ClassAnalysis), pointer                            :: AnalysisSettings
        type (ClassNodes) , pointer , dimension(:)               :: GlobalNodesList
        type (ClassElementsWrapper) , pointer , dimension(:)     :: ElementList
        class (ClassBoundaryConditions), pointer                 :: BC
        class (ClassNonlinearSolver) , pointer                   :: NLSolver
        character(len=*)                                         :: FileName

        !************************************************************************************

        !************************************************************************************
        ! SELECT PARAMETERS OF THE analysis type
        !************************************************************************************

        allocate(AnalysisSettings)

        ! Reading the input files
        !************************************************************************************
        call ReadInputFile( FileName, AnalysisSettings , GlobalNodesList , ElementList , &
                            BC , NLSolver )


        if (AnalysisSettings%MultiscaleAnalysis) then
            allocate( ClassMultiscaleAnalysis :: Analysis)
        else
            if (AnalysisSettings%ProblemType .eq. ProblemTypes%Mechanical) then
                allocate( ClassFEMAnalysis :: Analysis)
                allocate(Analysis%Kg)
            elseif (AnalysisSettings%ProblemType .eq. ProblemTypes%Biphasic) then
                allocate( ClassFEMAnalysisBiphasic :: Analysis)
                allocate(Analysis%Kg)
                allocate(Analysis%KgFluid)
            else
                stop 'Error: Problem Type not identified in ReadAndCreateAnalysis'
            endif
        endif

        Analysis%AnalysisSettings => AnalysisSettings
        Analysis%GlobalNodesList => GlobalNodesList
        Analysis%ElementList => ElementList
        Analysis%BC => BC
        Analysis%NLSolver => NLSolver

        !allocate(Analysis%Kg)

        !************************************************************************************

    end subroutine


end module
