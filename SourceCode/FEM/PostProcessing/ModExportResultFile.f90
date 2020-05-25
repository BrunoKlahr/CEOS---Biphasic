!##################################################################################################
! This module has the procedures for pos processing
!--------------------------------------------------------------------------------------------------
! Date: 2014/02
!
! Authors:  Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!           
!!------------------------------------------------------------------------------------------------
! Modifications: 
! Date:    2019     Author: Bruno Klahr
!##################################################################################################
module ModExportResultFile

    use ModFEMAnalysis
    use ModFEMAnalysisBiphasic
    use ModProbe
    use ModPostProcessors
    use ModGid
    use ModHyperView


    contains

    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  ReadPostProcessingInputFile(FileName,ProbeList,PostProcessor)
        ! TODO (Thiago#2#): Organizar melhor a lógica de leitura dos Probes.


        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        character (len=*) :: FileName
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        class(ClassPostProcessor), pointer :: PostProcessor

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type (ClassParser) :: File
        character(len=255) :: OptionName, OptionValue, String, ProbeHyperMeshFile, ProbeLoadCollector
        character(len=255) :: ProbeLocation, ProbeFileName, ProbeVariableName, ProbeComponentsString
        logical :: ProbeAllComponents

        character(len=255), allocatable, dimension(:) :: PostProcessorResults
        character(len=255)                            :: PostProcessorFileName=''

        integer :: NumberOfProbes, ProbeNode, ProbeElement, ProbeGaussPoint, i

        !************************************************************************************

        call File%Setup (FileName,FileNumber=30)

        write(*,*) 'Reading Post Processing File: ',trim(FileName)


        ! Leitura do Pos Processador
        !------------------------------------------------------------------------------------
        call File%GetNextString(String)

        if ( .not. File%CompareStrings(String,'POST PROCESSOR') ) then
            call File%RaiseError('Expecting Word POST PROCESSOR in '//trim(FileName))
        end if


        call File%GetNextOption(OptionName,OptionValue)

        if (File%CompareStrings(OptionName,'Post Processor')) then

            ! GiD 7
            !========================================================================================
            if ( File%CompareStrings(OptionValue,'GiD 7') ) then

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'Results') ) then
                    call File%RaiseError('Expecting Word Results in '//trim(FileName))
                end if

                call Split( OptionValue , PostProcessorResults , ",")

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'File Name') ) then
                    call File%RaiseError('Expecting Word File Name in '//trim(FileName))
                end if
                PostProcessorFileName = OptionValue

                ! Constuindo o Pos Processador GiD 7
                !------------------------------------------------------------------------------------
                call Constructor_GiD( PostProcessor, PostProcessorResults, PostProcessorFileName )

                call File%GetNextString(String)

                if ( .not. File%CompareStrings(String,'END POST PROCESSOR') ) then
                    call File%RaiseError('Expecting Word END POST PROCESSOR in '//trim(FileName))
                end if

            ! HyperView 12
            !========================================================================================
            elseif ( File%CompareStrings(OptionValue,'HyperView 12') ) then

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'Results') ) then
                    call File%RaiseError('Expecting Word Results in '//trim(FileName))
                end if

                call Split( OptionValue , PostProcessorResults , ",")

                call File%GetNextOption(OptionName,OptionValue)
                if ( .not. File%CompareStrings(OptionName,'File Name') ) then
                    call File%RaiseError('Expecting Word File Name in '//trim(FileName))
                end if
                PostProcessorFileName = OptionValue

                ! Constuindo o Pos Processador HiperView 12
                !------------------------------------------------------------------------------------
                call Constructor_HyperView( PostProcessor, PostProcessorResults, PostProcessorFileName )

                call File%GetNextString(String)

                if ( .not. File%CompareStrings(String,'END POST PROCESSOR') ) then
                    call File%RaiseError('Expecting Word END POST PROCESSOR in '//trim(FileName))
                end if

            ! Nenhum Pós Processodor
            !========================================================================================
            elseif ( File%CompareStrings(OptionValue,'None') ) then

                call File%GetNextString(String)
                do while (.not. File%CompareStrings(String,'END POST PROCESSOR'))
                    call File%GetNextString(String)
                enddo

            endif
            !========================================================================================

        else
            call File%RaiseError('Expecting Post Processor Name in '//trim(FileName))

        end if




        !Começo da leitura do arquivo de Probes
        call File%GetNextOption(OptionName,OptionValue)


        if (File%CompareStrings(OptionName,'Number of Probes')) then

            NumberOfProbes = OptionValue
        else
            call File%RaiseError('Expecting Number of Probes in '//trim(FileName))
        end if

        allocate (ProbeList(NumberOfProbes))


        do i = 1,NumberOfProbes

            call File%GetNextString(String)

            if (.not. File%CompareStrings(String,'Probe')) then
                call File%RaiseError('Expecting Word PROBE in '//trim(FileName))
            end if

            ProbeLocation = ''
            ProbeFileName = ''
            ProbeVariableName = ''
            ProbeComponentsString = ''
            NumberOfProbes = 0
            ProbeNode = 0
            ProbeElement = 0
            ProbeGaussPoint = 0
            ProbeHyperMeshFile = ''
            ProbeLoadCollector = ''


            PROBE_BLOCK_LOOP: do while (.true.)

                call File%GetNextString(String)

                if (File%CompareStrings(String,'End Probe')) then
                    exit PROBE_BLOCK_LOOP
                end if
                OptionValue = ''
                call File%GetCurrentOption(OptionName,OptionValue)

                if (File%CompareStrings(OptionName,'Location')) then
                    ProbeLocation = OptionValue

                elseif (File%CompareStrings(OptionName,'File Name')) then
                    ProbeFileName = OptionValue

                elseif (File%CompareStrings(OptionName,'Variable Name')) then
                    ProbeVariableName = OptionValue

                elseif (File%CompareStrings(OptionName,'Node')) then
                    ProbeNode = OptionValue

                elseif (File%CompareStrings(OptionName,'Components')) then
                    ProbeComponentsString = OptionValue

                elseif (File%CompareStrings(OptionName,'Element')) then
                    ProbeElement = OptionValue

                elseif (File%CompareStrings(OptionName,'Gauss Point')) then
                    ProbeGaussPoint = OptionValue

                elseif (File%CompareStrings(OptionName,'HyperMesh File')) then
                    ProbeHyperMeshFile = OptionValue

                elseif (File%CompareStrings(OptionName,'Load Collector')) then
                    ProbeLoadCollector = OptionValue

                else
                    call File%RaiseError('Expression not identified in '//trim(FileName))
                endif

            enddo PROBE_BLOCK_LOOP



        ! Construtor dos Probes
        ! Probes de Nós
        if (  File%CompareStrings(ProbeLocation, 'Node' ) ) then

            call NodeProbeConstructor(ProbeList(i)%Pr, ProbeFileName, ProbeVariableName, ProbeNode, ProbeComponentsString)

        ! Probes de Pontos de Gauss
        elseif (  File%CompareStrings(ProbeLocation, 'Gauss Point' ) ) then

            call GaussPointProbeConstructor(ProbeList(i)%Pr, ProbeVariableName, ProbeElement, ProbeFileName, ProbeGaussPoint, ProbeComponentsString)

        ! Probes de Forças Nodais
        elseif (  File%CompareStrings(ProbeLocation, 'Nodal Force' ) ) then

            call NodalForceProbeConstructor(ProbeList(i)%Pr, ProbeHyperMeshFile, ProbeFileName, ProbeLoadCollector, ProbeComponentsString)

        ! Probes de Micro Estrutura
        elseif (  File%CompareStrings(ProbeLocation, 'Micro Structure' ) ) then

            call MicroStructureProbeConstructor(ProbeList(i)%Pr, ProbeVariableName, ProbeFileName, ProbeComponentsString)
        
        ! Probes de Macro Estrutura
        elseif (  File%CompareStrings(ProbeLocation, 'Macro Structure' ) ) then

            call MacroStructureProbeConstructor(ProbeList(i)%Pr, ProbeVariableName, ProbeFileName, ProbeComponentsString)

        endif

    end do

        !************************************************************************************

    end subroutine
    !==========================================================================================



    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  PostProcessingResults(ProbeList,PostProcessor,FEA)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        use Interfaces
        use ModStatus
        use ModIO
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        class (ClassPostProcessor), pointer             :: PostProcessor
        class (ClassFEMAnalysis)                        :: FEA

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type(ClassParser)                         :: ResultFile
        type(ClassStatus)                         :: Status
        integer :: TotalNDOF, LoadCase, Step, CutBack, SubStep, el, gp, i, FileNumber
        real(8) :: Time
        real(8) , allocatable, target, dimension(:) :: U
        character(len=255) :: OptionName, OptionValue, String, FileName
        integer :: Flag_EndStep, NumberOfIterations,IterationFile


        !************************************************************************************

        write(*,*) 'Post Processing Results...'

        ! Analisar se existem os arquivos dos probes pedidos. Caso existam, são deletados
        do i = 1, size(ProbeList)
            call ProbeList(i)%Pr%InitializeFile
        enddo

        if (associated(PostProcessor)) then
            call PostProcessor%InitializePostProcessorFile(FEA)
        endif

        IterationFile = FreeFile()
        open(IterationFile,File='NumberOfIterationsToConverge.dat',status='unknown')
        write(IterationFile,*)'  Time                    Number Of Iterations To Converge'

        FileName='FEMAnalysis.result'
        FileNumber = 222
        call ResultFile%Setup(FileName,FileNumber)

        call ResultFile%GetNextOption(OptionName,OptionValue)

        TotalNDOF = OptionValue

        allocate( U(TotalNDOF) )


        call FEA%AdditionalMaterialModelRoutine()

        ! Restart Constitutive Model
        ! -----------------------------------------------------------------------------------
        do el = 1,size(FEA%ElementList)
            do gp = 1,size(FEA%ElementList(el)%El%GaussPoints)
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelDestructor()
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelConstructor(FEA%AnalysisSettings)
            enddo
        enddo

        ! Restart Mesh Coordinates
        ! -----------------------------------------------------------------------------------
        do i = 1,size(FEA%GlobalNodesList)
            FEA%GlobalNodesList(i)%Coord = FEA%GlobalNodesList(i)%CoordX
        enddo


        LOOP_TIME :do while (.true.)


            call ResultFile%GetNextOption(OptionName,OptionValue)

            if (EOF(ResultFile)) exit LOOP_TIME

            Time = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            LoadCase = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Step = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            CutBack = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Substep = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            Flag_EndStep = OptionValue

            call ResultFile%GetNextOption(OptionName,OptionValue)

            NumberOfIterations = OptionValue

            do i = 1, TotalNDOF
                call ResultFile%GetNextString(String)
                U(i) = String
            enddo

            FEA%LoadCase = LoadCase
            FEA%Time = Time
            FEA%U => U
            ! Update stress and internal variables
            
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!   Troca da ordem na atualização das coordenadas - Bruno 02/08
            ! Update Coordinates
            if (FEA%AnalysisSettings%NLAnalysis == .true.) then
                call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            endif
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call SolveConstitutiveModel( FEA%ElementList , FEA%AnalysisSettings, Time, U, Status)

            ! Escrevendo os pontos pedidos. Excluindo soluções dos Cut Backs.
            if (Flag_EndStep .eq. 1) then
                do i = 1, size(ProbeList)
                    call ProbeList(i)%Pr%WriteProbeResult(FEA)
                enddo
                if (associated(PostProcessor)) then
                    call PostProcessor%WritePostProcessorResult(FEA)
                endif
                write(IterationFile,*)Time,NumberOfIterations
            endif

            ! SAVING THE CONVERGED STATE
            ! ----------------------------------------------------------------------------------
            do el=1,size(FEA%ElementList)
                do gp=1,size(FEA%ElementList(el)%el%GaussPoints)
                    call FEA%ElementList(el)%el%GaussPoints(gp)%SwitchConvergedState()
                enddo
            enddo

            ! Update Coordinates
            !if (FEA%AnalysisSettings%NLAnalysis == .true.) then
            !    call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            !endif



        enddo LOOP_TIME

        call ResultFile%CloseFile

        close(IterationFile)

    end subroutine
    !==========================================================================================


    !==========================================================================================
    ! Subroutine Description:
    !==========================================================================================
    subroutine  PostProcessingResultsBiphasic(ProbeList,PostProcessor,FEA)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use Parser
        use Interfaces
        use ModStatus
        use ModIO
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------
        type (ClassProbeWrapper), pointer, dimension(:) :: ProbeList
        class (ClassPostProcessor), pointer             :: PostProcessor
        class (ClassFEMAnalysis)                        :: FEA

        ! Internal variables
        ! -----------------------------------------------------------------------------------
        type(ClassParser)                         :: ResultFileSolid
        type(ClassParser)                         :: ResultFileFluid
        type(ClassStatus)                         :: Status
        integer :: TotalNDOF_Solid, LoadCase, Step, CutBack, SubStep, el, gp, i
        integer :: TotalNDOF_Fluid, FileNumberFluid, FileNumberSolid
        real(8) :: Time
        real(8) , allocatable, target, dimension(:) :: U , P, Psolid
        character(len=255) :: OptionName, OptionValue, String
        character(len=255) :: FileNameSolid, FileNameFluid
        integer :: Flag_EndStep, NumberOfIterations,IterationFile


        !************************************************************************************

        write(*,*) 'Post Processing Results...'

        ! Analisar se existem os arquivos dos probes pedidos. Caso existam, são deletados
        do i = 1, size(ProbeList)
            call ProbeList(i)%Pr%InitializeFile
        enddo

        if (associated(PostProcessor)) then
            call PostProcessor%InitializePostProcessorFile(FEA)
        endif

        IterationFile = FreeFile()
        open(IterationFile,File='NumberOfIterationsToConverge.dat',status='unknown')
        write(IterationFile,*)'  Time                    Number Of Iterations To Converge'

        FileNameSolid='FEMAnalysisSolid.result'
        FileNumberSolid = 222
        FileNameFluid='FEMAnalysisFluid.result'
        FileNumberFluid = 223
        
        call ResultFileSolid%Setup(FileNameSolid,FileNumberSolid)
        call ResultFileFluid%Setup(FileNameFluid,FileNumberFluid)

        call ResultFileSolid%GetNextOption(OptionName,OptionValue)
        TotalNDOF_Solid = OptionValue
        call ResultFileFluid%GetNextOption(OptionName,OptionValue)
        TotalNDOF_Fluid = OptionValue

        allocate( U(TotalNDOF_Solid) )
        allocate( P(TotalNDOF_Fluid) )
        allocate( Psolid(size(FEA%GlobalNodesList)) )
        Psolid = 0.0d0

        call FEA%AdditionalMaterialModelRoutine()

        ! Restart Constitutive Model
        ! -----------------------------------------------------------------------------------
        do el = 1,size(FEA%ElementList)
            do gp = 1,size(FEA%ElementList(el)%El%GaussPoints)
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelDestructor()
                call FEA%ElementList(el)%El%GaussPoints(gp)%ConstitutiveModelConstructor(FEA%AnalysisSettings)
            enddo
        enddo

        ! Restart Mesh Coordinates
        ! -----------------------------------------------------------------------------------
        do i = 1,size(FEA%GlobalNodesList)
            FEA%GlobalNodesList(i)%Coord = FEA%GlobalNodesList(i)%CoordX
        enddo


        LOOP_TIME :do while (.true.)


            call ResultFileSolid%GetNextOption(OptionName,OptionValue)           
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)
            
            if (EOF(ResultFileSolid) .and. EOF(ResultFileFluid)) exit LOOP_TIME

            Time = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            LoadCase = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            Step = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            CutBack = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            Substep = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            Flag_EndStep = OptionValue

            call ResultFileSolid%GetNextOption(OptionName,OptionValue)
            call ResultFileFluid%GetNextOption(OptionName,OptionValue)

            NumberOfIterations = OptionValue

            do i = 1, TotalNDOF_Solid
                call ResultFileSolid%GetNextString(String)
                U(i) = String
            enddo
            
            ! P -> pressão nos nós do fluido
            do i = 1, TotalNDOF_Fluid
                call ResultFileFluid%GetNextString(String)
                P(i) = String
            enddo

            call InterpolatePFluidToPSolid(FEA, P , Psolid)
            
            FEA%LoadCase = LoadCase
            FEA%Time = Time
            FEA%U => U
            FEA%P => Psolid  ! É necessário fazer uma conta para obter pressão em todos os nós do sólido
            
            ! Update stress and internal variables
            
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!   Troca da ordem na atualização das coordenadas - Bruno 02/08
            ! Update Coordinates
            if (FEA%AnalysisSettings%NLAnalysis == .true.) then
                call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            endif
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            call SolveConstitutiveModel( FEA%ElementList , FEA%AnalysisSettings, Time, U, Status)
            
            call SolveVelocidadeRelativaW( FEA%ElementList , FEA%AnalysisSettings, Time, P, Status)
            
            ! Escrevendo os pontos pedidos. Excluindo soluções dos Cut Backs.
            if (Flag_EndStep .eq. 1) then
                do i = 1, size(ProbeList)
                    call ProbeList(i)%Pr%WriteProbeResult(FEA)
                enddo
                if (associated(PostProcessor)) then
                    call PostProcessor%WritePostProcessorResult(FEA)
                endif
                write(IterationFile,*)Time,NumberOfIterations
            endif

            ! SAVING THE CONVERGED STATE
            ! ----------------------------------------------------------------------------------
            do el=1,size(FEA%ElementList)
                do gp=1,size(FEA%ElementList(el)%el%GaussPoints)
                    call FEA%ElementList(el)%el%GaussPoints(gp)%SwitchConvergedState()
                enddo
            enddo

            ! Update Coordinates
            !if (FEA%AnalysisSettings%NLAnalysis == .true.) then
            !    call UpdateMeshCoordinates(FEA%GlobalNodesList,FEA%AnalysisSettings,U)
            !endif



        enddo LOOP_TIME

        call ResultFileSolid%CloseFile
        call ResultFileFluid%CloseFile

        close(IterationFile)

    end subroutine 
    !==========================================================================================

    subroutine InterpolatePFluidToPSolid(FEA, P,Psolid)
    
        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        implicit none


        ! Input variables
        ! -----------------------------------------------------------------------------------  
        class (ClassFEMAnalysis)                        :: FEA
        real(8) , dimension(:)                          :: P
            
        ! Output variables
        ! -----------------------------------------------------------------------------------  
        real(8) , dimension(:)                          :: Psolid
        
        ! Inernal variables
        class(ClassElementBiphasic), pointer :: ElBiphasic
        integer                                         :: TotalNDOF_Solid, nDOFel_fluid
        integer     :: i, j, k, Elem
        real(8)                                         :: Pinterpolated
        real(8) , pointer , dimension(:)                :: Pe
        integer , pointer , dimension(:)                :: GM_fluid
        real(8), dimension(3,10)                        :: NodalNaturalCoordT10
        real(8), dimension(3)                           :: NaturalCoord
        
        TotalNDOF_Solid = size(Psolid)
        call ConvertElementToElementBiphasic(FEA%ElementList(1)%El,  ElBiphasic)
        call ElBiphasic%GetElementNumberDOF_fluid(FEA%AnalysisSettings, nDOFel_fluid)
        
        Pe => Pe_Memory(1:nDOFel_fluid)
        GM_fluid => GMfluid_Memory(1:nDOFel_fluid)
        Pe = 0.0d0
        
        ! Definindo o vetor de NaturalCoord para os nós do T10
        NodalNaturalCoordT10 = 0.0d0
        NodalNaturalCoordT10(1,2)=1.0d0
        NodalNaturalCoordT10(2,3)=1.0d0
        NodalNaturalCoordT10(3,4)=1.0d0
        NodalNaturalCoordT10(1,5)=0.5d0
        NodalNaturalCoordT10(1,6)=0.5d0
        NodalNaturalCoordT10(2,6)=0.5d0
        NodalNaturalCoordT10(2,7)=0.5d0
        NodalNaturalCoordT10(3,8)=0.5d0
        NodalNaturalCoordT10(1,9)=0.5d0
        NodalNaturalCoordT10(3,9)=0.5d0
        NodalNaturalCoordT10(2,10)=0.5d0
        NodalNaturalCoordT10(3,10)=0.5d0
        
        do i=1, size(FEA%GlobalNodesList)
            
            if (FEA%GlobalNodesList(i)%IDFluid .ne. 0) then
                    Psolid(i) = P(FEA%GlobalNodesList(i)%IDFluid)
            else
                do j=1, size(FEA%ElementList)
                    
                    do k=1, size(FEA%ElementList(j)%El%ElementNodes)
                    
                        if (i .eq. FEA%ElementList(j)%El%ElementNodes(k)%Node%ID) then
                            Elem = j
                            exit
                        end if   
                    end do
                    if (k .ne. size(FEA%ElementList(j)%El%ElementNodes)+1) then
                        exit
                    end if   
                end do
                
                call ConvertElementToElementBiphasic(FEA%ElementList(Elem)%El,  ElBiphasic) ! Aponta o objeto ElBiphasic para o ElementList(e)%El mas com o type correto ClassElementBiphasic

                call ElBiphasic%GetGlobalMapping_fluid(FEA%AnalysisSettings, GM_fluid)
                Pe = P(GM_fluid)
          
                NaturalCoord = NodalNaturalCoordT10(:,k)

                call ElBiphasic%ElementInterpolation_fluid(Pe, NaturalCoord, Pinterpolated)
                Psolid(i) = Pinterpolated
                
            endif
            
        end do
        
        
    end subroutine
    
    subroutine SolveVelocidadeRelativaW( ElementList , AnalysisSettings, Time, P, Status)

        !************************************************************************************
        ! DECLARATIONS OF VARIABLES
        !************************************************************************************
        ! Modules and implicit declarations
        ! -----------------------------------------------------------------------------------
        use ElementLibrary
        use ModAnalysis
        use ModStatus


        implicit none

        ! Input variables
        ! -----------------------------------------------------------------------------------
        type(ClassElementsWrapper) , dimension(:)  :: ElementList
        type(ClassAnalysis)                        :: AnalysisSettings
        type(ClassStatus)                          :: Status
        real(8)                    , dimension(:)  :: P
        real(8)                                    :: Time

        ! Internal variables
        ! -----------------------------------------------------------------------------------

        integer :: e , gp , nDOFel_Fluid
        integer , pointer , dimension(:)   :: GM_fluid
        real(8) , pointer , dimension(:,:) :: NaturalCoord
        real(8) , pointer , dimension(:)   :: Weight
        class(ClassElementBiphasic), pointer :: ElBiphasic
        real(8) , pointer , dimension(:) :: Pe
        real(8) , pointer , dimension(:,:)  :: Kf, H
        real(8)							    :: detJ, FactorAxi
  

        !************************************************************************************

        !************************************************************************************
        ! COMPUTING RELATIVE VELOCITY
        !************************************************************************************

        !$OMP PARALLEL DEFAULT(PRIVATE) FIRSTPRIVATE(Status) SHARED(ElementList, AnalysisSettings, P, Time)
        !$OMP DO
        do e = 1 , size(ElementList)
            call ConvertElementToElementBiphasic(ElementList(e)%el,  ElBiphasic) ! Aponta o objeto ElBiphasic para o ElementList(e)%El mas com o type correto ClassElementBiphasic
            call ElBiphasic%GetElementNumberDOF_fluid(AnalysisSettings, nDOFel_fluid)
            GM_fluid => GMfluid_Memory(1:nDOFel_fluid)
            Pe => Pe_Memory(1:nDOFel_fluid)
            call ElBiphasic%GetGaussPoints_Fluid(NaturalCoord,Weight)
            call ElBiphasic%GetGlobalMapping_Fluid(AnalysisSettings,GM_Fluid)
           
            
            ! Allocating matrix H
            H => H_Memory( 1:3 , 1:NDOFel_fluid )

            ! Allocating permeability tensor
            Kf => Kf_Memory(1:3, 1:3)
            
            Pe = P(GM_fluid)

            ! Loop over the Gauss Points
            do gp = 1 , size(ElBiphasic%GaussPoints)
                
                !Get the permeability k of the Gauss Point
                Kf = 0.0d0
                call ElBiphasic%GaussPoints(gp)%GetPermeabilityTensor(Kf)

                !Get matrix H
                call ElBiphasic%MatrixH_ThreeDimensional(AnalysisSettings, NaturalCoord(gp,:), H, detJ , FactorAxi)

                ElBiphasic%GaussPoints(gp)%AdditionalVariables%w= -matmul(matmul(Kf , H), Pe)
                
            enddo
        enddo
        !$OMP END DO
        !$OMP END PARALLEL

    end subroutine


end module
