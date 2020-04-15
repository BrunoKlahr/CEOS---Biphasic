!##################################################################################################
! This module has the system of equations of  FEM for the Solid (Biphasic Analysis)
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
module modFEMSystemOfEquationsSolid

    use ModNonLinearSystemOfEquations
    use ModAnalysis
    use BoundaryConditions
    use ElementLibrary
    use GlobalSparseMatrix

    implicit none

    type , extends(ClassNonLinearSystemOfEquations) :: ClassFEMSystemOfEquationsSolid

        real(8),dimension(:),allocatable                       :: Fint , Fext , UBar
        real(8),dimension(:),allocatable                       :: Pfluid     !Global pressure of biphasic material
        real(8)                                                :: Time
        integer                      , dimension(:) , pointer  :: DispDOF

        integer, dimension(:), allocatable                     :: PrescDispSparseMapZERO
        integer, dimension(:), allocatable                     :: PrescDispSparseMapONE
        integer, dimension(:), allocatable                     :: FixedSupportSparseMapZERO
        integer, dimension(:), allocatable                     :: FixedSupportSparseMapONE

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
        class(ClassFEMSystemOfEquationsSolid) :: this
        real(8),dimension(:) :: X,R

            !X -> Global Solid displacement    
        
            ! Update stress and internal variables (Se o modelo constitutivo depende da Pressão, precisa atualizar o SolveConstitutiveModel)
            call SolveConstitutiveModel( this%ElementList , this%AnalysisSettings , this%Time, X, this%Status)

            ! Constitutive Model Failed. Used for Cut Back Strategy
            if (this%Status%Error ) then
                return
            endif

            ! Internal Force
            call InternalForceSolid(this%ElementList , this%AnalysisSettings , this%Pfluid, this%Fint, this%Status)

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
        class(ClassFEMSystemOfEquationsSolid)        :: this
        class (ClassGlobalSparseMatrix), pointer     :: G
        real(8),dimension(:)                         :: X , R
        real(8)                                      :: norma

        call TangentStiffnessMatrixSolid(this%AnalysisSettings , this%ElementList , this%Pfluid , this%Kg )

        ! As CC de deslocamento prescrito estão sendo aplicadas no sistema Kx=R e não em Kx=-R!!!
        R = -R
        !call this%BC%ApplyBoundaryConditions(  this%Kg , R , this%DispDOF, this%Ubar , X   )
        call this%BC%ApplyBoundaryConditionsNEW(  this%Kg , R , this%DispDOF, this%Ubar , X, this%PrescDispSparseMapZERO, this%PrescDispSparseMapONE, this%FixedSupportSparseMapZERO, this%FixedSupportSparseMapONE )
        R = -R

        G => this%Kg

    end subroutine

!--------------------------------------------------------------------------------------------------

    subroutine FEMUpdateMesh(this,X)
        use Interfaces
        class(ClassFEMSystemOfEquationsSolid) :: this
        real(8),dimension(:)::X

        if (this%AnalysisSettings%NLAnalysis == .true.) then
            call UpdateMeshCoordinates(this%GlobalNodesList,this%AnalysisSettings,X)
        endif

    end subroutine





end module

