!##################################################################################################
! This module has the system of equations of  FEM for the Fluid (Biphasic Analysis)
!--------------------------------------------------------------------------------------------------
! Date: 2019/05
!
! Authors:  Bruno Klahr
!           Thiago Andre Carniel
!           
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:  
!                               
!##################################################################################################
module modFEMSystemOfEquationsFluid

    use ModNonLinearSystemOfEquations
    use ModAnalysis
    use BoundaryConditions
    use ElementLibrary
    use GlobalSparseMatrix

    implicit none

    type , extends(ClassNonLinearSystemOfEquations) :: ClassFEMSystemOfEquationsFluid

        real(8),dimension(:),allocatable                       :: Fint , Fext , PBar  !(Pbar = Ubar)
        real(8),dimension(:),allocatable                       :: VSolid ! Global solid velocity
        real (8)                                               :: Time
        integer                      , dimension(:) , pointer  :: PresDOF

        integer, dimension(:), allocatable                     :: PrescPresSparseMapZERO
        integer, dimension(:), allocatable                     :: PrescPresSparseMapONE
       !integer, dimension(:), allocatable                     :: FixedSupportSparseMapZERO ! Não existe para o fluído
       !integer, dimension(:), allocatable                     :: FixedSupportSparseMapONE  ! Não existe para o fluído   

        type (ClassElementsWrapper)  , dimension(:) , pointer  :: ElementList
        type (ClassNodes)            , dimension(:) , pointer  :: GlobalNodesList
        type (ClassAnalysis)                                   :: AnalysisSettings
        class (ClassBoundaryConditions)             , pointer  :: BC
        type (ClassGlobalSparseMatrix)              , pointer  :: Kg


    contains

        procedure :: EvaluateSystem => EvaluateR
        procedure :: EvaluateGradientSparse => EvaluateKt
        procedure :: PostUpdate => FEMUpdateMesh

    end type

    contains
!--------------------------------------------------------------------------------------------------
    subroutine EvaluateR(this,X,R)

        use Interfaces
        class(ClassFEMSystemOfEquationsFluid) :: this
        real(8),dimension(:) :: X,R

            ! Update stress and internal variables ***********************************************************
            !call SolveConstitutiveModel( this%ElementList , this%AnalysisSettings , this%Time, X, this%Status)

            ! Constitutive Model Failed. Used for Cut Back Strategy
            !if (this%Status%Error ) then
            !    return
            !endif
            
            ! X -> Global pressure of biphasic material
            ! Internal Force
            call InternalForceFluid(this%ElementList , this%AnalysisSettings , X , this%VSolid , this%Fint , this%Status)

            ! det(Jacobian Matrix)<=0 .Used for Cut Back Strategy
            if (this%Status%Error ) then
                return
            endif

            ! Residual
            R = this%Fint - this%Fext

    end subroutine

!--------------------------------------------------------------------------------------------------

    subroutine EvaluateKt(this,X,R,G)

        use Interfaces
        use MathRoutines
        class(ClassFEMSystemOfEquationsFluid)        :: this
        class (ClassGlobalSparseMatrix), pointer :: G
        real(8),dimension(:) :: X , R
        real(8) :: norma

        call TangentStiffnessMatrixFluid(this%AnalysisSettings , this%ElementList , this%Kg )

        ! As CC de deslocamento prescrito estão sendo aplicadas no sistema Kx=R e não em Kx=-R!!!
        R = -R
        !****************************************************************************************
        !****************************************************************************************
        !call this%BC%ApplyBoundaryConditions(  this%Kg , R , this%DispDOF, this%Ubar , X   )
        call this%BC%ApplyBoundaryConditionsFluid(  this%Kg , R , this%PresDOF, this%Pbar , X, this%PrescPresSparseMapZERO, this%PrescPresSparseMapONE)
        !****************************************************************************************
        !****************************************************************************************
        R = -R

        G => this%Kg

    end subroutine

!--------------------------------------------------------------------------------------------------

    subroutine FEMUpdateMesh(this,X)
        use Interfaces
        class(ClassFEMSystemOfEquationsFluid) :: this
        real(8),dimension(:)::X

        ! Fluid do not update the mesh
        
        !if (this%AnalysisSettings%NLAnalysis == .true.) then
        !    call UpdateMeshCoordinates(this%GlobalNodesList,this%AnalysisSettings,X)
        !endif

    end subroutine





end module

