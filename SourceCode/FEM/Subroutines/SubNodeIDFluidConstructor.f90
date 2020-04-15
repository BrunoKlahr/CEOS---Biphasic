!##################################################################################################
! This routine construct the IDFluid for the GlobalNodeList. (Biphasic Analysis)
!--------------------------------------------------------------------------------------------------
! Date: 2019/05
!
! Authors:  Bruno Klahr
!           Thiago A. Carniel
    
!!------------------------------------------------------------------------------------------------
! Modifications:
! Date:         Author:
!##################################################################################################
subroutine NodeIDFluidConstructor( ElementList, GlobalNodesList )

    !************************************************************************************
    ! DECLARATIONS OF VARIABLES
    !************************************************************************************
    ! Modules and implicit declarations
    ! -----------------------------------------------------------------------------------
    use ModAnalysis
    use ElementLibrary
    use Interfaces
    use Nodes


    implicit none

    ! Input variables
    ! -----------------------------------------------------------------------------------
    type (ClassNodes) , pointer , dimension(:)                      :: GlobalNodesList
    type (ClassElementsWrapper) , pointer , dimension(:)            :: ElementList

    ! Output variables
    ! -----------------------------------------------------------------------------------
    !GlobalNodeList%IDFluid

    ! Internal variables
    ! -----------------------------------------------------------------------------------
    integer :: e, i, j, k, s, i1, Flag
    integer nNodes_fluid
    integer, allocatable, dimension(:) :: jVetor
    class(ClassElementBiphasic), pointer :: ElBiphasic

    !************************************************************************************
    k = 1
    i1 = 1
    allocate( jVetor(size(ElementList)*4))
    jVetor = 0.0d0
    do e = 1, size(ElementList)
        call ConvertElementToElementBiphasic(ElementList(e)%el,  ElBiphasic) ! Aponta o objeto ElBiphasic para o ElementList(e)%El mas com o type correto ClassElementBiphasic
        nNodes_fluid = ElBiphasic%GetNumberOfNodes_fluid()
        do i = 1, nNodes_fluid
            j = ElBiphasic%ElementNodes_fluid(i)%Node%ID  ! ID Global do nó 
            jVetor(i1) = j
            s=1  
            Flag=1
            do while ( s .LT. i1)
                if (j .eq. jVetor(s)) then ! Procura se o nó j já foi adicionado como um nó de fluído
                    Flag = 0
                    s=i1                  
                endif
                s=s+1
            enddo        
            i1 = i1+1
            if (Flag .eq. 1) then
                GlobalNodesList(j)%IDFluid = k
                k = k+1
            endif                    
        enddo    
    enddo
    
    !************************************************************************************

end subroutine

