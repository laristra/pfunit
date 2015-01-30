
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: ExpectationPolyWrap
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 29 Jan 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 29 Jan 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------


module ExpectationPolyWrap_mod
      use Expectation_mod, only: Expectation   
   implicit none
   private

   public ExpectationPolyWrap

   type ExpectationPolyWrap
      class(Expectation), allocatable :: item
   contains
      procedure :: get

#ifdef __INTEL_COMPILER
      procedure :: copy
      generic :: assignment(=) => copy
#endif

   end type ExpectationPolyWrap

   interface ExpectationPolyWrap
      module procedure new_copy
   end interface ExpectationPolyWrap


contains


   function new_copy(item) result(container)
      type (ExpectationPolyWrap) :: container
      class(Expectation), intent(in) :: item
      allocate(container%item, source=item)
   end function new_copy

   
   function get(this) result(item)
      class (ExpectationPolyWrap), target, intent(in) :: this
      class(Expectation), pointer :: item

      item => this%item

   end function get


#ifdef __INTEL_COMPILER
   subroutine copy(a, b)
      class(ExpectationPolyWrap), intent(out) :: a
      class(ExpectationPolyWrap), intent(in) :: b

      allocate(a%item, source=b%item)
   end subroutine copy
#endif


end module ExpectationPolyWrap_mod

   
