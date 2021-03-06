!##################################################################################################
! This module has a FEM Analysis Biphasic (Biphasic Analysis)
!--------------------------------------------------------------------------------------------------
! Date: 2019/05
!
! Authors:  Bruno Klahr
!           Jan-Michel Farias
!           Thiago Andre Carniel
!           Paulo Bastos de Castro
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author: 
!##################################################################################################
module ModFEMAnalysisBiphasic

	! Modules and implicit declarations
	! ---------------------------------------------------------------------------------------------
    use ElementLibrary
    use Nodes
    use ModAnalysis
    use BoundaryConditions
    use GlobalSparseMatrix
    use NonLinearSolver
    use ModFEMAnalysis

    implicit none


	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassFEMAnalysisBiphasic: Definitions of FEM analysis Biphasic
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type, extends (ClassFEMAnalysis) :: ClassFEMAnalysisBiphasic

		! Class Attributes
		!----------------------------------------------------------------------------------------    
        !type  (ClassGlobalSparseMatrix) , pointer                    :: KgFluid

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure :: Solve => SolveFEMAnalysisBiphasic
            procedure :: AllocateKgSparseUpperTriangular => AllocateKgSparseUpperTriangularBiphasic 
            !----------------------------------------------------------------------------------      

    end type

    contains



        !##################################################################################################
        ! This routine pre-allocates the size of the global stiffness matrix in the sparse format.
        !--------------------------------------------------------------------------------------------------
        ! Date: 2019/05
        !
        ! Authors:  Bruno Klahr
        !           Jan-Michel Farias
        !           Thiago Andre Carniel
        !           Paulo Bastos de Castro
        !!------------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:

        !##################################################################################################
        subroutine AllocateKgSparseUpperTriangularBiphasic (this)

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use SparseMatrixRoutines
            use ModAnalysis
            use Element
            use GlobalSparseMatrix

            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysisBiphasic) :: this

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            type(SparseMatrix) :: KgSparseSolid
            type(SparseMatrix) :: KgSparseFluid
            class(ClassElementBiphasic), pointer :: ElBiphasic
            real(8) , pointer , dimension(:,:)  :: KeSolid, KeFluid
            integer , pointer , dimension(:)    :: GMSolid, GMFluid
            integer ::  e, nDOFel_solid, nDOF_solid
            integer ::  nDOFel_fluid, nDOF_fluid
            

            !************************************************************************************


            !************************************************************************************
            ! PRE-ALLOCATING THE GLOBAL STIFFNESS MATRIX
            !************************************************************************************

            !Allocating memory for the sparse matrix (pre-assembling)
            !************************************************************************************
            call this%AnalysisSettings%GetTotalNumberOfDOF (this%GlobalNodesList, nDOF_solid)
            call this%AnalysisSettings%GetTotalNumberOfDOF_fluid (this%GlobalNodesList, nDOF_fluid)

            !Element stiffness matrix used to allocate memory (module Analysis)
            Ke_Memory = 1.0d0    ! Solid Element stiffness matrix
            KeF_Memory = 1.0d0   ! Fluid Element stiffness matrix

            !Initializing the sparse global stiffness matrix
            call SparseMatrixInit( KgSparseSolid , nDOF_solid )
            call SparseMatrixInit( KgSparseFluid , nDOF_fluid )

            !Loop over elements to mapping the local-global positions in the sparse stiffness matrix
            do e=1,size( this%ElementList )
                call ConvertElementToElementBiphasic(this%ElementList(e)%el,  ElBiphasic) ! Aponta o objeto ElBiphasic para o ElementList(e)%El mas com o type correto ClassElementBiphasic
                call ElBiphasic%GetElementNumberDOF(this%AnalysisSettings , nDOFel_solid)
                call ElBiphasic%GetElementNumberDOF_fluid(this%AnalysisSettings , nDOFel_fluid)


                KeSolid => Ke_Memory( 1:nDOFel_solid , 1:nDOFel_solid )
                GMSolid => GM_Memory( 1:nDOFel_solid )
                KeFluid => KeF_Memory( 1:nDOFel_fluid , 1:nDOFel_fluid)
                GMFluid => GMfluid_Memory( 1:nDOFel_fluid)

                call ElBiphasic%GetGlobalMapping( this%AnalysisSettings, GMSolid )
                call ElBiphasic%GetGlobalMapping_fluid( this%AnalysisSettings, GMFluid )

                call SparseMatrixSetArray( GMSolid, GMSolid, KeSolid, KgSparseSolid, OPT_SET )
                call SparseMatrixSetArray( GMFluid, GMFluid, KeFluid, KgSparseFluid, OPT_SET )
                
            enddo

            !Converting the sparse matrix to coordinate format (used by Pardiso Sparse Solver)
            call ConvertToCoordinateFormatUpperTriangular( KgSparseSolid , this%Kg%Row , this%Kg%Col , this%Kg%Val , this%Kg%RowMap) ! this%Kg -> Matriz de rigidez do Solid
            call ConvertToCoordinateFormatUpperTriangular( KgSparseFluid , this%KgFluid%Row , this%KgFluid%Col , this%KgFluid%Val , this%KgFluid%RowMap) ! this%KgFluid -> Matriz de rigidez do Fluid

            !Releasing memory
            call SparseMatrixKill(KgSparseSolid)
            call SparseMatrixKill(KgSparseFluid)

            !************************************************************************************

        end subroutine
        !##################################################################################################



        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  SolveFEMAnalysisBiphasic( this )

		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            class(ClassFEMAnalysisBiphasic) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            integer :: nDOF

 		    !************************************************************************************
            ! SELECT PARAMETERS OF THE analysis type
		    !************************************************************************************


            ! Calling the quasi-static analysis routine
            !************************************************************************************
            select case ( this%AnalysisSettings%AnalysisType )

                case ( AnalysisTypes%Quasi_Static )
                               
                    call this%AdditionalMaterialModelRoutine()

                    call QuasiStaticAnalysisFEM_biphasic( this%ElementList, this%AnalysisSettings, this%GlobalNodesList , &
                                                  this%BC, this%Kg, this%KgFluid, this%NLSolver )
                             
                case default
                    stop "Error in AnalysisType - ModFEMAnalysis"
            end select



		    !************************************************************************************

        end subroutine
        !==========================================================================================



        !==========================================================================================
        ! Method ClassFEMAnalysis:
        !------------------------------------------------------------------------------------------
        ! Modifications:
        ! Date:         Author:
        !==========================================================================================
        subroutine  WriteFEMResultsBiphasic(  U, TimeSolid,  P, TimeFluid, LC, ST, SubStep,FileIDSolid, FileIDFluid, NumberOfIterations)
        
		    !************************************************************************************
            ! DECLARATIONS OF VARIABLES
		    !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            implicit none

            ! Object
            ! -----------------------------------------------------------------------------------
            !class(ClassFEMAnalysis) :: this

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8) :: TimeSolid, TimeFluid
            real(8), dimension(:) :: U, P
            integer :: FileIDSolid, FileIDFluid, LC, ST,SubStep
            
            ! Internal Variables
            integer :: i 
            integer :: CutBack, Flag_EndStep, NumberOfIterations
            
            ! Dummy Variables just to don't change the write (  ************)
            CutBack = 0
            Flag_EndStep = 1 ! Significa fim do STEP
            !NumberOfIterations = 0
 		    
            !************************************************************************************
            ! WRITING SOLID RESULTS
		    !************************************************************************************
            write(FileIDSolid,*) 'TIME =', TimeSolid
            write(FileIDSolid,*) 'LOAD CASE =', LC
            write(FileIDSolid,*) 'STEP =', ST
            write(FileIDSolid,*) 'CUT BACK =', CutBack
            write(FileIDSolid,*) 'SUBSTEP =', SubStep
            write(FileIDSolid,*) 'FLAG END STEP =', Flag_EndStep
            write(FileIDSolid,*) 'NUMBER OF ITERATIONS TO CONVERGE =', NumberOfIterations
            do i = 1,size(U)
                write(FileIDSolid,*) U(i)
            enddo
		    !************************************************************************************
            ! WRITING FLUID RESULTS
		    !************************************************************************************
            write(FileIDFluid,*) 'TIME =', TimeFluid
            write(FileIDFluid,*) 'LOAD CASE =', LC
            write(FileIDFluid,*) 'STEP =', ST
            write(FileIDFluid,*) 'CUT BACK =', CutBack
            write(FileIDFluid,*) 'SUBSTEP =', SubStep
            write(FileIDFluid,*) 'FLAG END STEP =', Flag_EndStep
            write(FileIDFluid,*) 'NUMBER OF ITERATIONS TO CONVERGE =', NumberOfIterations
            do i = 1,size(P)
                write(FileIDFluid,*) P(i)
            enddo
		    !************************************************************************************

        end subroutine
        !==========================================================================================

        !##################################################################################################
        ! This routine contains the procedures to solve a quasi-static analysis based in a incremental-
        ! iterative approach for the biphasic model (Solid + Fluid)
        !##################################################################################################
        subroutine QuasiStaticAnalysisFEM_biphasic( ElementList , AnalysisSettings , GlobalNodesList , BC  , &
                                            KgSolid , KgFluid, NLSolver )

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use ElementLibrary
            use ModAnalysis
            use Nodes
            use BoundaryConditions
            use GlobalSparseMatrix
            use NonLinearSolver
            use Interfaces
            use MathRoutines
            use LoadHistoryData
            use modFEMSystemOfEquations
            use modFEMSystemOfEquationsSolid
            use modFEMSystemOfEquationsFluid

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type (ClassAnalysis)                                    :: AnalysisSettings
            type (ClassElementsWrapper),     pointer, dimension(:)  :: ElementList
            type (ClassNodes),               pointer, dimension(:)  :: GlobalNodesList
            class (ClassBoundaryConditions), pointer                :: BC
            class(ClassNonLinearSolver),     pointer                :: NLSolver
            
            !************************************************************************************
            type (ClassGlobalSparseMatrix),  pointer                :: KgSolid        !  Kg Solid
            type (ClassGlobalSparseMatrix),  pointer                :: KgFluid        !  Kg Fluid
            !************************************************************************************

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8), allocatable, dimension(:) :: U , RSolid , DeltaFext, DeltaUPresc, Fext_alpha0, Ubar_alpha0, Uconverged        ! Solid
            real(8), allocatable, dimension(:) :: VSolid , VSolidconverged, ASolidconverged, ASolid                                                         ! Solid
            real(8), allocatable, dimension(:) :: P , RFluid , DeltaFluxExt, DeltaPPresc, FluxExt_alpha0, Pbar_alpha0, Pconverged  ! Fluid
            real(8), allocatable, dimension(:) :: Ustaggered, Pstaggered   ! Internal variables of staggered prcedure
            real(8) :: DeltaTime , Time_alpha0
            real(8) :: NormStagSolid, NormStagFluid, TolSTSolid, TolSTFluid, InitialNormStagSolid, InitialNormStagFluid, InitialNormStagMin
            real(8) :: alpha !, alpha_max, alpha_min, alpha_aux
            integer :: LC , ST , nSteps, nLoadCases , SubStep, e, gp
            integer :: FileID_FEMAnalysisResultsSolid, FileID_FEMAnalysisResultsFluid
           !integer :: CutBack, Flag_EndStep
            integer :: nDOFSolid, nDOFFluid
            real(8), parameter :: GR= (1.0d0 + dsqrt(5.0d0))/2.0d0

            integer, allocatable, dimension(:) :: KgSolidValZERO, KgSolidValONE
            integer :: contZEROSolid, contONESolid
            integer, allocatable, dimension(:) :: KgFluidValZERO, KgFluidValONE
            integer :: contZEROFluid, contONEFluid
            integer :: SubstepsMAX
            integer :: Phase ! Indicates the material phase (1 = Solid; 2 = Fluid)

            type(ClassFEMSystemOfEquationsSolid) :: FEMSoESolid
            type(ClassFEMSystemOfEquationsFluid) :: FEMSoEFluid

            FileID_FEMAnalysisResultsSolid = 42
            open (FileID_FEMAnalysisResultsSolid,file='FEMAnalysisSolid.result',status='unknown')
            FileID_FEMAnalysisResultsFluid = 43
            open (FileID_FEMAnalysisResultsFluid,file='FEMAnalysisFluid.result',status='unknown')

            !************************************************************************************

            !************************************************************************************
            ! QUASI-STATIC ANALYSIS
            !***********************************************************************************
            call AnalysisSettings%GetTotalNumberOfDOF (GlobalNodesList, nDOFSolid)
            call AnalysisSettings%GetTotalNumberOfDOF_fluid (GlobalNodesList, nDOFFluid)

            write(FileID_FEMAnalysisResultsSolid,*) 'Total Number of Solid DOF  = ', nDOFSolid
            write(FileID_FEMAnalysisResultsFluid,*) 'Total Number of Fluid DOF  = ', nDOFFluid

            ! Definitions of FEMSoESolid
            FEMSoESolid % ElementList => ElementList
            FEMSoESolid % AnalysisSettings = AnalysisSettings
            FEMSoESolid % GlobalNodesList => GlobalNodesList
            FEMSoESolid % BC => BC
            FEMSoESolid % Kg => KgSolid
            
            ! Definitions of FEMSoEFluid
            FEMSoEFluid % ElementList => ElementList
            FEMSoEFluid % AnalysisSettings = AnalysisSettings
            FEMSoEFluid % GlobalNodesList => GlobalNodesList
            FEMSoEFluid % BC => BC
            FEMSoEFluid % Kg => KgFluid
            
            ! Allocate the FEMSoESolid
            allocate( FEMSoESolid% Fint(nDOFSolid) , FEMSoESolid% Fext(nDOFSolid) , FEMSoESolid% Ubar(nDOFSolid), FEMSoESolid% Pfluid(nDOFFluid) )
            ! Allocate the FEMSoEFluid
            allocate( FEMSoEFluid% Fint(nDOFFluid) , FEMSoEFluid% Fext(nDOFFluid) , FEMSoEFluid% Pbar(nDOFFluid), FEMSoEFluid% VSolid(nDOFSolid) )


            ! Allocating Solid arrays 
            allocate(RSolid(nDOFSolid) , DeltaFext(nDOFSolid), Fext_alpha0(nDOFSolid))
            allocate( U(nDOFSolid)  , DeltaUPresc(nDOFSolid), Ubar_alpha0(nDOFSolid), Uconverged(nDOFSolid)  )
            allocate( VSolid(nDOFSolid),  VSolidconverged(nDOFSolid), ASolidconverged(nDOFSolid), ASolid(nDOFSolid) )
            ! Allocating Fluid arrays
            allocate(RFluid(nDOFFluid) , DeltaFluxExt(nDOFFluid), FluxExt_alpha0(nDOFFluid))
            allocate( P(nDOFFluid)  , DeltaPPresc(nDOFFluid), Pbar_alpha0(nDOFFluid), Pconverged(nDOFFluid)  )
            ! Allocating staggered variables
            allocate( Ustaggered(nDOFSolid) , Pstaggered(nDOFFluid)   )

            SubstepsMAX = 1000
            U = 0.0d0
            Ubar_alpha0 = 0.0d0
            VSolid = 0.0d0
            ASolid = 0.0d0
            P = 0.0d0
            Pbar_alpha0 = 0.0d0

            ! Staggered variables
            NormStagSolid       = 0.0d0
            NormStagFluid       = 0.0d0
            TolSTSolid          = 1.0d-3
            TolSTFluid          = 1.0d-3
            InitialNormStagMin  = 1.0d-12
            
            nLoadCases = BC%GetNumberOfLoadCases() !Verificar

            ! Escrevendo os resultados para o tempo zero
            ! NOTE (Thiago#1#11/19/15): OBS.: As condi��es de contorno iniciais devem sair do tempo zero.
    
            call WriteFEMResultsBiphasic( U, 0.0d0,  P, 0.0d0, 1, 1, 0,FileID_FEMAnalysisResultsSolid,FileID_FEMAnalysisResultsFluid, 0)


            !LOOP - LOAD CASES
            LOAD_CASE:  do LC = 1 , nLoadCases

                write(*,'(a,i3)')'Load Case: ',LC
                write(*,*)''

                nSteps = BC%GetNumberOfSteps(LC)

               ! LOOP - STEPS
                STEPS:  do ST = 1 , nSteps

                    write(*,'(4x,a,i3,a,i3,a)')'Step: ',ST,' (LC: ',LC,')'
                    write(*,*)''

                    call BC%GetBoundaryConditions(AnalysisSettings, GlobalNodesList,  LC, ST, Fext_alpha0, DeltaFext,FEMSoESolid%DispDOF, U, DeltaUPresc)
                    call BC%GetBoundaryConditionsFluid(AnalysisSettings, GlobalNodesList,  LC, ST, FluxExt_alpha0, DeltaFluxExt,FEMSoEFluid%PresDOF, P, DeltaPPresc)

                    !-----------------------------------------------------------------------------------
                    ! Mapeando os graus de liberdade da matrix esparsa para a aplica��o das CC de Dirichlet
                    
                    if ( (LC == 1) .and. (ST == 1) ) then
                        !-----------------------------------------------------------------------------------
                        ! Condi��o de contorno de deslocamento prescrito
                        allocate( KgSolidValZERO(size(FEMSoESolid%Kg%Val)), KgSolidValONE(size(FEMSoESolid%Kg%Val)) )

                        call BC%AllocatePrescDispSparseMapping(FEMSoESolid%Kg, FEMSoESolid%DispDOF, KgSolidValZERO, KgSolidValONE, contZEROSolid, contONESolid)

                        allocate( FEMSoESolid%PrescDispSparseMapZERO(contZEROSolid), FEMSoESolid%PrescDispSparseMapONE(contONESolid) )

                        FEMSoESolid%PrescDispSparseMapZERO(:) = KgSolidValZERO(1:contZEROSolid)
                        FEMSoESolid%PrescDispSparseMapONE(:)  = KgSolidValONE(1:contONESolid)

                        call BC%AllocateFixedSupportSparseMapping(FEMSoESolid%Kg, KgSolidValZERO, KgSolidValONE, contZEROSolid, contONESolid)

                        allocate( FEMSoESolid%FixedSupportSparseMapZERO(contZEROSolid), FEMSoESolid%FixedSupportSparseMapONE(contONESolid) )

                        FEMSoESolid%FixedSupportSparseMapZERO(:) = KgSolidValZERO(1:contZEROSolid)
                        FEMSoESolid%FixedSupportSparseMapONE(:)  = KgSolidValONE(1:contONESolid)

                        deallocate( KgSolidValZERO, KgSolidValONE )
                        
                        !-----------------------------------------------------------------------------------
                        ! Condi��o de contorno de press�o prescrita
                        allocate( KgFluidValZERO(size(FEMSoEFluid%Kg%Val)), KgFluidValONE(size(FEMSoEFluid%Kg%Val)) )
                        
                        call BC%AllocatePrescPresSparseMapping(FEMSoEFluid%Kg, FEMSoEFluid%PresDOF, KgFluidValZERO, KgFluidValONE, contZEROFluid, contONEFluid)
                        
                        allocate( FEMSoEFluid%PrescPresSparseMapZERO(contZEROFluid), FEMSoEFluid%PrescPresSparseMapONE(contONEFluid) )
                        
                        FEMSoEFluid%PrescPresSparseMapZERO(:) = KgFluidValZERO(1:contZEROFluid)
                        FEMSoEFluid%PrescPresSparseMapONE(:)  = KgFluidValONE(1:contONEFluid)
                        
                        deallocate( KgFluidValZERO, KgFluidValONE )
                        
                        
                        !-----------------------------------------------------------------------------------
                        ! Calculando Velocidade inicial para os GDL de deslocamento prescrito
                        
                    end if
                    !-----------------------------------------------------------------------------------

                    
                    call BC%GetTimeInformation(LC,ST,Time_alpha0,DeltaTime) !Verificar



                    
                   ! if ( (LC == 1) .and. (ST == 1) ) then
                        !-----------------------------------------------------------------------------------
                        ! Calculando Velocidade inicial para os GDL de deslocamento prescrito
                  !      VSolidconverged = (DeltaUPresc - Uconverged)/DeltaTime
                   ! end if
                    
                    
                    ! Prescribed Incremental Displacement
                    Ubar_alpha0 = U
                    ! Prescribed Incremental Pressure
                    Pbar_alpha0 = P
                    ! Switch Displacement Converged
                    Uconverged = U
                    VSolidconverged = VSolid
                    ASolidconverged = ASolid
                    
               !     if ( (LC == 1) .and. (ST == 1) ) then
               !         !-----------------------------------------------------------------------------------
               !         ! Calculando campo de press�o inicial
               !          call Compute_Initial_Pressure(NLSolver, nDOFSolid, nDOFFluid, FEMSoESolid, FEMSoEFluid, Time_alpha0, DeltaTime, Fext_alpha0, DeltaFext, &
               !                                     Ubar_alpha0, DeltaUPresc, FluxExt_alpha0, DeltaFluxExt, Pbar_alpha0, DeltaPPresc,  U, P)
               !     end if
                    
                    ! Switch Pressure Converged
                    Pconverged = P

                    
                    !-----------------------------------------------------------------------------------
                    ! Vari�veis do CutBack  - alpha = passo no step
                    !alpha_max = 1.0d0 ; alpha_min = 0.0d0
                    !alpha = alpha_max
                    !CutBack = 0
                    alpha = 1.0d0   ! passo no step
                    !-----------------------------------------------------------------------------------
                    
                    SubStep = 1

                    write(*,'(12x,a)') 'Begin of Staggered procedure '
                    SUBSTEPS: do while(.true.)   !Staggered procedure

                        
                        ! Update the staggerd variables
                        Ustaggered = U
                        Pstaggered = P

                        !write(*,'(8x,a,i3)') 'Cut Back: ',CutBack
                        !write(*,'(12x,a,i3,a,f7.4,a)') 'SubStep: ',SubStep,' (Alpha: ',alpha,')'
                        write(*,'(12x,a,i3)') 'SubStep: ',SubStep

                        ! -----------------------------------------------------------------------------------
                        ! Solve the Solid System of Equations
                        FEMSoESolid % Time = Time_alpha0 + alpha*DeltaTime
                        FEMSoESolid % Fext = Fext_alpha0 + alpha*DeltaFext
                        FEMSoESolid % Ubar = Ubar_alpha0 + alpha*DeltaUPresc
                        FEMSoESolid % Pfluid = Pstaggered    !Pconverged

                        write(*,'(12x,a)') 'Solve the Solid system of equations '
                        call NLSolver%Solve( FEMSoESolid , XGuess = Ustaggered , X = U, Phase = 1 )

                        IF (NLSolver%Status%Error) then
                            write(*,'(12x,a)') 'Solid Not Converged - '//Trim(NLSolver%Status%ErrorDescription)
                            write(*,'(12x,a)') Trim(FEMSoESolid%Status%ErrorDescription)
                            write(*,*)''
                            pause
                        ENDIF
                        
                      !  if ( (LC == 1) .and. (ST == 1) ) then
                            !-----------------------------------------------------------------------------------
                            ! Calculando Velocidade inicial 
                      !      VSolidconverged = (U - Uconverged)/DeltaTime
                      !  end if
                        
                        ! -----------------------------------------------------------------------------------
                        ! Update the Solid Velocity via Newmark's equation
                        call ComputeVelocity(DeltaTime, Uconverged, U, VSolidconverged, VSolid, ASolidconverged, ASolid)
                        
                        
                        ! -----------------------------------------------------------------------------------
                        ! Solve the Fluid System of Equations
                        FEMSoEFluid % Time = Time_alpha0 + alpha*DeltaTime
                        FEMSoEFluid % Fext = Fext_alpha0 + alpha*DeltaFluxExt
                        FEMSoEFluid % Pbar = Pbar_alpha0 + alpha*DeltaPPresc
                        FEMSoEFluid % VSolid = VSolid
                        
                        write(*,'(12x,a)') 'Solve the Fluid system of equations '
                        call NLSolver%Solve( FEMSoEFluid , XGuess = Pstaggered , X = P, Phase = 2 )

                        IF (NLSolver%Status%Error) then
                            write(*,'(12x,a)') 'Fluid Not Converged - '//Trim(NLSolver%Status%ErrorDescription)
                            write(*,'(12x,a)') Trim(FEMSoEFluid%Status%ErrorDescription)
                            write(*,*)''
                            pause
                        ENDIF
                        
                        
                        ! -----------------------------------------------------------------------------------
                        ! Convergence criterion
                        NormStagSolid = maxval(dabs(Ustaggered-U))
                        NormStagFluid = maxval(dabs(Pstaggered-P))
                        
                        ! Obtaining the initial Norm for the Staggered convergence criterion                        
                        if (LC .eq. 1 .and. ST .eq. 1 .and. subStep .eq. 1) then
                            InitialNormStagSolid = maxval(dabs(Ustaggered-U))
                            if (InitialNormStagSolid .lt. InitialNormStagMin) then 
                                InitialNormStagSolid = InitialNormStagMin
                            endif
                            InitialNormStagFluid = maxval(dabs(Pstaggered-P))
                            if (InitialNormStagFluid .lt. InitialNormStagMin) then 
                                InitialNormStagFluid = InitialNormStagMin
                            endif
                        endif
                        
                        ! Teste bisse��o (mean pressure)
                        if (NormStagSolid .ne. 0) then
                            P = (Pstaggered+P)/2
                        endif
                        
                        !U = (Ustaggered+U)/2
                        !P = (Ustaggered+P)/2
                    
                     
                        
                      !  if (ST == 1 .and.  subStep < 10) then
                      !      P = 0.1*subStep*P
                      ! endif
                        
                        
                      ! if (ST == 1 .and. subStep < 15 .and. subStep >= 8) then
                      !     P = 0.003*substep*P
                      ! endif
                      ! 
                      ! if (ST == 1 .and. subStep < 25 .and. subStep >= 15) then
                      !     P = 0.02*substep*P
                      ! endif
                        
                       
                        
                        
                        if (NormStagSolid .lt. InitialNormStagSolid*TolSTSolid .and. NormStagFluid .lt. InitialNormStagFluid*TolSTFluid) then
                            write(*,'(12x,a,i3,a)') 'Staggered procedure converged in', SubStep ,' substeps'
                            write(*,'(12x,a,i3,a,i3)') 'Step', ST ,' of Load Case', LC
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                            exit SUBSTEPS
                        elseif (Substep .ge. SubstepsMAX) then
                            write(*,'(12x,a)') 'Error: Maximum Number of Iterations of staggered procedure is reached!'
                            write(*,'(12x,a,i3,a,i3)') 'Error in Step', ST ,' of Load Case', LC
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                            stop
                        else 
                            write(*,'(12x,a,i3,a,i3)') 'Step', ST ,' of Load Case', LC
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                            write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                            SubStep = SubStep + 1
                            
                        endif     
                        
                        
                        
                    
                         
                    enddo SUBSTEPS

                    ! -----------------------------------------------------------------------------------
                    ! Write the results
                    call WriteFEMResultsBiphasic( U, FEMSoESolid%Time,  P, FEMSoEFluid%Time, LC, ST, SubStep,FileID_FEMAnalysisResultsSolid, &
                                                  FileID_FEMAnalysisResultsFluid, Substep)

                    ! -----------------------------------------------------------------------------------
                    ! SWITCH THE CONVERGED STATE: StateVariable_n := StateVariable_n+1
                    ! -----------------------------------------------------------------------------------
                    do e=1,size(elementlist)
                        do gp=1,size(elementlist(e)%el%GaussPoints)
                            call ElementList(e)%el%GaussPoints(gp)%SwitchConvergedState()               !!! VERIFICAR
                        enddo
                    enddo
                    ! -----------------------------------------------------------------------------------

                    write(*,'(4x,a,i3)')'End Step: ',ST
                    write(*,*)''

                enddo STEPS

                write(*,'(a,i3)')'End Load Case: ',LC
                write(*,*)''
                write(*,*)''

            enddo LOAD_CASE

            close (FileID_FEMAnalysisResultsSolid)
            close (FileID_FEMAnalysisResultsFluid)
            !************************************************************************************

        end subroutine

        !###########################################################################################
        subroutine ComputeVelocity(DeltaTime, Uconverged, U, VSolidconverged, VSolid, ASolidconverged, ASolid)
       
            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            real(8), dimension(:) ::  Uconverged, U , VSolidconverged, ASolidconverged
            real(8) :: DeltaTime
            
            ! Output variables
            ! -----------------------------------------------------------------------------------
            real(8), dimension(:) ::  VSolid, ASolid 
            real(8) ::  aux1, aux2, aux3, aux4, aux5, aux6
            
            
            
            ! M�todo de Newmark
            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8) :: Gamma, Beta  ! Par�metros do procedimento de Newmark ******
            real(8) :: omega        ! Par�metro da regra do trap�zio ******
            
            Gamma = 0.5d0   ! Impl�cito e incondicionalmente est�vel
            Beta  = 0.25d0
            omega = 0.5d0   ! Impl�cito e incondicionalmente est�vel
            
            
            ! Update the solid aceleration
            !aux1 = (1/(Beta*(DeltaTime**2)))*(U(1)-Uconverged(1))
            !aux2 = (1/(Beta*DeltaTime))*VSolidconverged(1) 
            !aux3 = ((0.5 - Beta)/Beta)*ASolidconverged(1)
           
            !ASolid = (1/(Beta*(DeltaTime**2)))*(U-Uconverged) -  (1/(Beta*DeltaTime))*VSolidconverged - ((0.5 - Beta)/Beta)*ASolidconverged  !*******
            
            ! Update the solid velocity         
            !VSolid  = VSolidconverged + DeltaTime*(1-Gamma)*ASolidconverged + Gamma*DeltaTime*ASolid
            
            ! Diferen�as Finitas
            VSolid = (U-Uconverged)/DeltaTime
            
            
            ! Regra do Trap�zio
            
            !VSolid = (U-Uconverged)/(DeltaTime*omega) - ((1-omega)/omega)*VSolidconverged
           
            
           ! aux4 = U(3*41)
           ! aux5 = VSolid(3*41)
           ! aux6 = ASolid(3*41)
            
            
        endsubroutine
        !###########################################################################################        

        
        !##################################################################################################
        ! This routine contains the procedures to solve a one step of quasi-static analysis based in a incremental-
        ! iterative approach for the biphasic model (Solid + Fluid). The goal is obtain a initial value of the
        ! pressure field.
        !##################################################################################################
        subroutine Compute_Initial_Pressure(NLSolver, nDOFSolid, nDOFFluid, FEMSoESolid, FEMSoEFluid, Time_alpha0, DeltaTime, Fext_alpha0, DeltaFext, Ubar_alpha0, DeltaUPresc,&
                                                    FluxExt_alpha0, DeltaFluxExt, Pbar_alpha0, DeltaPPresc,   U, P)

            !************************************************************************************
            ! DECLARATIONS OF VARIABLES
            !************************************************************************************
            ! Modules and implicit declarations
            ! -----------------------------------------------------------------------------------
            use ElementLibrary
            use ModAnalysis
            use Nodes
            use BoundaryConditions
            use GlobalSparseMatrix
            use NonLinearSolver
            use Interfaces
            use MathRoutines
            use LoadHistoryData
            use modFEMSystemOfEquations
            use modFEMSystemOfEquationsSolid
            use modFEMSystemOfEquationsFluid

            implicit none

            ! Input variables
            ! -----------------------------------------------------------------------------------
            type(ClassFEMSystemOfEquationsSolid)                    :: FEMSoESolid
            type(ClassFEMSystemOfEquationsFluid)                    :: FEMSoEFluid
            class(ClassNonLinearSolver),     pointer                :: NLSolver
            real(8),  dimension(:) :: U , DeltaFext, DeltaUPresc, Fext_alpha0, Ubar_alpha0       ! Solid
            real(8),  dimension(:) :: P , DeltaFluxExt, DeltaPPresc, FluxExt_alpha0, Pbar_alpha0 ! Fluid
            real(8) :: DeltaTime , Time_alpha0
            integer :: nDOFSolid, nDOFFluid
            
            
            ! Output variables
            ! -----------------------------------------------------------------------------------
 
            

            ! Internal variables
            ! -----------------------------------------------------------------------------------
            real(8), allocatable, dimension(:) :: RSolid , Uconverged                                   ! Solid
            real(8), allocatable, dimension(:) :: VSolid , VSolidconverged, ASolidconverged, ASolid     ! Solid
            real(8), allocatable, dimension(:) :: RFluid , Pconverged                                   ! Fluid
            real(8), allocatable, dimension(:) :: Ustaggered, Pstaggered   ! Internal variables of staggered prcedure
            
            real(8) :: NormStagSolid, NormStagFluid, TolSTSolid, TolSTFluid, InitialNormStagSolid, InitialNormStagFluid
            real(8) :: alpha!, alpha_max, alpha_min, alpha_aux
            integer :: LC , ST , nSteps, nLoadCases , SubStep, e, gp
  
            integer :: beta         ! Par�metro multiplicativo do Delta t
            integer :: SubstepsMAX
            integer :: Phase        ! Indicates the material phase (1 = Solid; 2 = Fluid)

            !************************************************************************************

            !************************************************************************************
            ! QUASI-STATIC ANALYSIS
            !***********************************************************************************
           

            ! Allocating Solid arrays 
            allocate( RSolid(nDOFSolid))
            allocate( Uconverged(nDOFSolid)  )
            allocate( VSolid(nDOFSolid),  VSolidconverged(nDOFSolid), ASolidconverged(nDOFSolid), ASolid(nDOFSolid) )
            ! Allocating Fluid arrays
            allocate( RFluid(nDOFFluid))
            allocate( Pconverged(nDOFFluid)  )
            ! Allocating staggered variables
            allocate( Ustaggered(nDOFSolid) , Pstaggered(nDOFFluid)   )

            SubstepsMAX = 500
            Uconverged = 0.0d0
            VSolid = 0.0d0
            VSolidconverged = 0.0d0
            ASolid = 0.0d0
            ASolidconverged = 0.0d0
            Pconverged = 0.0d0
            
            ! Staggered variables
            NormStagSolid = 0.0d0
            NormStagFluid = 0.0d0
            TolSTSolid    = 1.0d-4
            TolSTFluid    = 1.0d-4
            
            LC = 1
            ST = 1
            
            alpha = 1.0d0     ! passo no step
            beta =  1.5       ! Fator do incremento de tempo
           
                   
            SubStep = 1
            write(*,'(12x,a)') 'Compute the initial pressure field:'
            write(*,'(12x,a)') '... '
            write(*,'(12x,a)') '... '
            
               !     if ( (LC == 1) .and. (ST == 1) ) then
                        !-----------------------------------------------------------------------------------
                        ! Calculando campo de press�o inicial
               !           Compute_Initial_Pressure(NLSolver, nDOFSolid, nDOFFluid, FEMSoESolid, FEMSoEFluid, Time_alpha0, DeltaTime, Fext_alpha0, DeltaFext, Ubar_alpha0, DeltaUPresc,&
               !                                     FluxExt_alpha0, DeltaFluxExt, Pbar_alpha0, DeltaPPresc,  U, P)
              !      end if

            write(*,'(12x,a)') 'Begin of Staggered procedure '
            SUBSTEPS: do while(.true.)   !Staggered procedure

                ! Update the staggerd variables
                Ustaggered = U
                Pstaggered = P

                write(*,'(12x,a,i3)') 'SubStep: ',SubStep

                ! -----------------------------------------------------------------------------------
                ! Solve the Solid System of Equations
                FEMSoESolid % Time = Time_alpha0 + alpha*beta*DeltaTime
                FEMSoESolid % Fext = Fext_alpha0 + alpha*beta*DeltaFext
                FEMSoESolid % Ubar = Ubar_alpha0 + alpha*beta*DeltaUPresc
                FEMSoESolid % Pfluid = Pstaggered    !Pconverged

                write(*,'(12x,a)') 'Solve the Solid system of equations '
                call NLSolver%Solve( FEMSoESolid , XGuess = Uconverged , X = U, Phase = 1 )

                IF (NLSolver%Status%Error) then
                    write(*,'(12x,a)') 'Solid Not Converged - '//Trim(NLSolver%Status%ErrorDescription)
                    write(*,'(12x,a)') Trim(FEMSoESolid%Status%ErrorDescription)
                    write(*,*)''
                    pause
                ENDIF
                
              !  if ( (LC == 1) .and. (ST == 1) ) then
                    !-----------------------------------------------------------------------------------
                    ! Calculando Velocidade inicial 
              !      VSolidconverged = (U - Uconverged)/DeltaTime
              !  end if
                
                ! -----------------------------------------------------------------------------------
                ! Update the Solid Velocity via Newmark's equation
                call ComputeVelocity(beta*DeltaTime, Uconverged, U, VSolidconverged, VSolid, ASolidconverged, ASolid)
                
                
                ! -----------------------------------------------------------------------------------
                ! Solve the Fluid System of Equations
                FEMSoEFluid % Time = Time_alpha0    + alpha*beta*DeltaTime
                FEMSoEFluid % Fext = FluxExt_alpha0 + alpha*beta*DeltaFluxExt
                FEMSoEFluid % Pbar = Pbar_alpha0    + alpha*beta*DeltaPPresc
                FEMSoEFluid % VSolid = VSolid
                
                write(*,'(12x,a)') 'Solve the Fluid system of equations '
                call NLSolver%Solve( FEMSoEFluid , XGuess = Pconverged , X = P, Phase = 2 )

                IF (NLSolver%Status%Error) then
                    write(*,'(12x,a)') 'Fluid Not Converged - '//Trim(NLSolver%Status%ErrorDescription)
                    write(*,'(12x,a)') Trim(FEMSoEFluid%Status%ErrorDescription)
                    write(*,*)''
                    pause
                ENDIF
                
                
                ! -----------------------------------------------------------------------------------
                ! Convergence criterion
                NormStagSolid = maxval(dabs(Ustaggered-U))
                NormStagFluid = maxval(dabs(Pstaggered-P))
                
                if (LC .eq. 1 .and. ST .eq. 1 .and. subStep .eq. 1) then
                    InitialNormStagSolid = maxval(dabs(U))
                    InitialNormStagFluid = maxval(dabs(P))
                endif
                                       
                
                
                if (NormStagSolid .lt. InitialNormStagSolid*TolSTSolid .and. NormStagFluid .lt. InitialNormStagFluid*TolSTFluid) then
                    write(*,'(12x,a,i3,a)') 'Staggered procedure converged in ', SubStep ,' substeps'
                    write(*,'(12x,a,i3,a,i3)') 'Step', ST ,' of Load Case', LC
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                    exit SUBSTEPS
                elseif (Substep .ge. SubstepsMAX) then
                    write(*,'(12x,a)') 'Error: Maximum Number of Iterations of staggered procedure is reached!'
                    write(*,'(12x,a,i3,a,i3)') 'Error in Step', ST ,' of Load Case', LC
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                    stop
                else 
                    write(*,'(12x,a,i3,a,i3)') 'Step', ST ,' of Load Case', LC
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_SOLID: ',NormStagSolid
                    write(*,'(12x,a,i3,a,e16.9)') 'Substep: ',SubStep ,'  NORM_FLUID: ',NormStagFluid
                    SubStep = SubStep + 1
                    
                endif     
            write(*,'(12x,a)') '----------------------------------------------------------------------------------'     
            enddo SUBSTEPS
                        
            write(*,'(12x,a)') '... '
            write(*,'(12x,a)') 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX '
            write(*,'(12x,a)') 'End of the Compute the initial pressure field:'
            
            !U = U/beta
            !P = P/beta
            

        end subroutine
        
end module


































