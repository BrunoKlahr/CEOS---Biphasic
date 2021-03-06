!##################################################################################################
! This module has the attributes and methods for a family of nonlinear solvers.
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
module NonlinearSolver

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! DECLARATIONS OF VARIABLES
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	! Modules and implicit declarations
	! --------------------------------------------------------------------------------------------
    use LinearSolverLibrary
    use modNonLinearSystemOfEquations
    use modStatus

	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    ! ClassNonlinearSolver: definitions of the nonlinear solver
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    type , abstract :: ClassNonlinearSolver

		! Class Attributes
		!-----------------------------------------------------------------------------------------
        !class(ClassSparseLinearSolver) , pointer :: LinearSolver => null()
        integer :: NumberOfIterations
        class(ClassLinearSolver) , pointer :: LinearSolver => null()
        type(ClassStatus) :: Status

        contains

            ! Class Methods
            !----------------------------------------------------------------------------------
            procedure(NonLinearSolve) , deferred :: Solve
            !procedure , deferred :: Constructor
            procedure (ReadParameters_NonLinear) , deferred :: ReadSolverParameters

    end type
	!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	abstract interface
        subroutine ReadParameters_NonLinear(this,DataFile)
            use parser
            import
            class(ClassNonLinearSolver) :: this
            type(ClassParser)::DataFile
        end subroutine
        subroutine NonLinearSolve(this,SOE,Xguess,X, Phase)
            import
            class(ClassNonLinearSolver) :: this
            class(ClassNonLinearSystemOfEquations):: SOE
            real(8),dimension(:)          :: Xguess , X
            integer :: Phase
        end subroutine
    end interface


    contains

!        subroutine ReadSolverParametersBase(this,DataFile)
!            use parser
!            class(ClassNonLinearSolver) :: this
!            type(ClassParser)::DataFile
!            stop "Error: Non Linear Solver not defined."
!        END subroutine
!
!        subroutine ConstructorBase (this)
!            class(ClassNonLinearSolver) :: this
!            stop "Error: Non Linear Solver not defined."
!        end subroutine
!
!        subroutine NonLinearSolveBase(this,SOE,Xguess,X)
!            class(ClassNonLinearSolver) :: this
!            class(ClassNonLinearSystemOfEquations):: SOE
!            real(8),dimension(:)          :: Xguess , X
!            stop "Error: Non Linear Solver not defined"
!        end subroutine




end module
