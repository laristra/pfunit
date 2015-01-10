!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MockRepository
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module MockRepository_mod
   use Expectation_mod, only : Expectation, newExpectation, ExpectationThat
!   use Expectation_mod, only : Subject, newSubject, newSubjectNameOnly
   use Expectation_mod, only : Subject, newSubject
   use Expectation_mod, only : Predicate, newPredicate
   use Expectation_mod, only : wasCalled, wasNotCalled, wasCalledOnce

   implicit none
   private

   public :: MockRepository
   public :: newMockRepository
   public :: MockRepositoryPointer

   public :: Predicate, wasCalled, wasNotCalled, wasCalledOnce

   public :: MAX_LEN_METHOD_NAME
   public :: MAX_LEN_CALL_REGISTRATION

   integer, parameter :: MAX_LEN_METHOD_NAME = 32
   integer, parameter :: MAX_LEN_CALL_REGISTRATION = 32
   type MockRepository
      ! mlr todo Allocatable strings are available...
      ! mlr todo test under nag
      character(len=MAX_LEN_METHOD_NAME) :: method = ' '
      type(Expectation) :: Expectations(2)
      integer :: lastExpectation = 0
      !   character(len=MAX_LEN_CALL_REGISTRATION) :: callRegistry(2) ! make dynamic...
      type(Expectation) :: callRegistry(2)
      integer :: lastRegistration = 0

    contains

      procedure :: countTimesCalled
      
      procedure :: verifyMocking
      !procedure :: expectCall
      !procedure :: hasCalled

      procedure :: expectCall
      procedure :: hasCalled

      generic   :: add => addExpectation_
      procedure :: addExpectation_
      
      generic   :: addExpectationThat => addExpectationThat_sub_
      generic   :: addExpectationThat => addExpectationThat_subNameOnly_

      procedure :: addExpectationThat_sub_
      procedure :: addExpectationThat_subNameOnly_

      generic   :: registerMockCallBy => registerMockCallBy_subName_

      procedure :: registerMockCallBy_subName_

      procedure :: verify

!      procedure :: verifyArguments

      ! final?
      ! mlr todo make a deep delete
      procedure :: delete

   end type MockRepository

   interface add_
      module procedure addExpectation_
   end interface add_
   
   interface addExpectationThat_
      module procedure addExpectationThat_sub_
      module procedure addExpectationThat_subNameOnly_
   end interface addExpectationThat_

   interface registerMockCallBy_
      module procedure registerMockCallBy_subName_
   end interface registerMockCallBy_

   interface
      subroutine subVoid
      end subroutine subVoid
   end interface

!   interface 
!      module procedure addExpectationThat_sub_
!   end interface

   type :: MockRepositoryContainer
      class (MockRepository), pointer :: ptr
   end type MockRepositoryContainer

   ! Global singleton mock repository pointer.
   !
   class (MockRepository), pointer :: MockRepositoryPointer => null()
   !
   ! Kluge because we recurse through testCases/Suites.
   ! Need to understand ramifications for MPI use.
   !
   integer, parameter :: MockRepositoryStack_MAX = 8
   integer            :: MockRepositoryStack_Top = 0
   type (MockRepositoryContainer), dimension(MockRepositoryStack_MAX) :: &
        & MockRepositoryStack

contains

!! Begin older code

  function newMockRepository() result(repository)
    use Exception_mod
      type (MockRepository), pointer :: repository
!      type (MockRepository), allocatable, target :: repository

!!??      if ( associated(MockRepositoryPointer) ) then
!!??         nullify(MockRepositoryPointer)
!!??! Sometimes TestResult%endRun isn't called!  Feature or bug?  2014-0922-2209-31-UTC MLR
!!??!         print *,'MockRepository.newMockRepository::ERROR::RepositoryAlreadyAllocated'
!!??      end if

      MockRepositoryStack_Top = MockRepositoryStack_Top + 1
      if (MockRepositoryStack_Top > MockRepositoryStack_MAX) then
         call throw('MockRepository%newMockRepository::ERROR::StackException-TooManyTestLevels')
      end if
      
      allocate(repository); MockRepositoryPointer => repository
      repository%lastExpectation  = 0
      repository%lastRegistration = 0

      ! push the MR onto the stack.
      MockRepositoryStack(MockRepositoryStack_Top)%ptr => repository
      
   end function newMockRepository

   subroutine delete (this)
     use Exception_mod
     class (MockRepository), intent(inout) :: this
     
     if (.not.associated(MockRepositoryPointer)) then
        call throw('MockRepository%delete::ERROR::RepositoryNotAllocated')
     else
        nullify(MockRepositoryPointer)

        ! Assume stack is okay coming in.
        nullify(MockRepositoryStack(MockRepositoryStack_Top)%ptr)

        ! pop the MR off the stack (decrement top)
        MockRepositoryStack_Top = MockRepositoryStack_Top - 1
        ! Careful with the logic... 0 is an empty stack... Just leave things alone...
        if (MockRepositoryStack_Top < 0) then
           call throw('MockRepository%newMockRepository::ERROR::StackException-Underflow')
        else if (MockRepositoryStack_Top > 0) then
           MockRepositoryPointer => MockRepositoryStack(MockRepositoryStack_Top)%ptr
        end if
        
     end if

   end subroutine delete

   subroutine verifyMocking(this, object)
      use Exception_mod
      class (MockRepository), intent(inout) :: this
      class (*) :: object
      
      if (trim(this%method) /= '') then
         call throw('Expected method not called: method1() on object of class MockSUT.')
      end if

!-!! Begin older code
!-
!-   subroutine verifyMocking(this, object)
!-      use Exception_mod
!-      class (MockRepository), intent(inout) :: this
!-      class (*) :: object
!-      
!-      if (trim(this%method) /= '') then
!-         call throw('Expected method not called: method1() on object of class MockSUT.')
!-      end if
!-
!-!      ! Only need to verify once. Finish it off...
!-!      call this%delete()
!-
!-   end subroutine verifyMocking
!-
!-   subroutine expectCall(this, obj, method)
!-      class (MockRepository), intent(inout) :: this
!-      class(*), intent(in) :: obj
!-      character(len=*), intent(in) :: method
!-
!-      this%method = method
!-   end subroutine expectCall
!-
!-   subroutine hasCalled(this, obj, method)
!-      class (MockRepository), intent(inout) :: this
!-      class(*), intent(in) :: obj
!-      character(len=*), intent(in) :: method
!-
!-      if (trim(method) == trim(this%method)) then
!-         this%method=''
!-      end if
!-   end subroutine hasCalled
!-
!-!! End older code

   end subroutine verifyMocking

   subroutine expectCall(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      this%method = method
   end subroutine expectCall

   subroutine hasCalled(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      if (trim(method) == trim(this%method)) then
         this%method=''
      end if
   end subroutine hasCalled

!!!   !! End older code

   subroutine addExpectation_(this,expectation_)
     class (MockRepository), intent(inout) :: this
     type(Expectation), intent(in) :: expectation_

     ! Fake implementation
     this%lastExpectation = this%lastExpectation + 1
     this%Expectations(this%lastExpectation) = expectation_
   end subroutine addExpectation_

   subroutine addExpectationThat_sub_(this,sub,pred)
     class (MockRepository), intent(inout) :: this
     procedure(subVoid), pointer, intent(in) :: sub
!     procedure(subVoid), pointer, intent(in) :: subptr
     type(Predicate), intent(in) :: pred
     type(Expectation) exp

     exp = newExpectation( &
          & newSubject( 'dummy-sub-name', sub), &
          & pred )

     ! <exp ok> & <enough space in list>
     this%lastExpectation = this%lastExpectation + 1
     this%Expectations(this%lastExpectation) = exp

   end subroutine addExpectationThat_sub_

   subroutine addExpectationThat_subNameOnly_(this,subName,pred)
     class (MockRepository), intent(inout) :: this
     character(len=*), intent(in) :: subName
!     procedure(subVoid), pointer, intent(in) :: sub
!     procedure(subVoid), pointer, intent(in) :: subptr
     type(Predicate), intent(in) :: pred
     type(Expectation) exp

     exp = newExpectation( &
          & newSubject( subName ), &
          & pred )

     ! <exp ok> & <enough space in list>
     this%lastExpectation = this%lastExpectation + 1
     this%Expectations(this%lastExpectation) = exp

   end subroutine addExpectationThat_subNameOnly_

   subroutine registerMockCallBy_subName_(this,subName)
     class (MockRepository), intent(inout) :: this
     character(len=*), intent(in) :: subName
!     print *,'reg200: ',subName,this%lastRegistration
     ! Can we includ the calling sub here? For a better comparison with our Exp. list?
     ! <if space in registry>
     this%lastRegistration = this%lastRegistration + 1
     this%callRegistry(this%lastRegistration) &
          & = newExpectation( & ! mlr todo Expectation --> foundAction -- "Result"
          &                  newSubject(subName), &
          &                  wasCalled )
   end subroutine registerMockCallBy_subName_

   subroutine verify(this)
      use Exception_mod
      class (MockRepository), intent(inout), target :: this
      integer iExp, iReg
      class (Expectation), pointer :: exp, reg
      logical ok
      integer nCalls

      call throw('MockRepository::verify not implemented.')
      return

      ! Go through expectation logic. Note:  Maybe use original list of strings approach.
      ! Need to work out more complex logic.  Ess. need logic analyzer.
      ! Eventually, expectations should probably be trees.

      ! Maybe rework the following into "expected vs. found" for greater alignment
      ! with existing usage in PFUNIT.  Also consider existing capabilities in PFUNIT.

! mlr todo -- verify should not be hardwired for the things it has to handle 
! todo -- refactor expectations & foundActionResult -- recall interpreter implementation
!
 
! TODO: move verification to the expectations themselves.
! cf. source/ExpectationWasCalled.F90 for a first attempt.
! TODO: Improve the expectation list.  Currently a 2 element array!?

      do iExp=1,this%lastExpectation  ! 'with' syntax?
         exp => this%Expectations(iExp)
         ok = .false.

!         if(exp%pred%name .eq. 'wasCalled')then
!            ok = .false.
!            do iReg=1,this%lastRegistration
!               reg = this%callRegistry(iReg)
!!               print *,'verify1000: ', &
!!                    & trim(exp%subj%name)//'='// &
!!                    & trim(reg%subj%name)//', '// &
!!                    & trim(exp%pred%name)//'='// &
!!                    & trim(reg%pred%name)//'.'
!               if(exp%subj%name .eq. reg%subj%name) then
!                  if(exp%pred%name .eq. reg%pred%name) then
!   !???               if(exp%pred .eq. reg%pred) then
!                     ok=.true.
!                  end if
!               end if
!            end do
!         end if
!
!         if(exp%pred%name .eq. 'wasNotCalled')then
!            ok = .true.
!            do iReg=1,this%lastRegistration
!               reg = this%callRegistry(iReg)
!               if(exp%subj%name .eq. reg%subj%name) then
!                  if(trim(reg%pred%name).eq.'wasCalled')then
!                     ok = .false.
!                  end if
!               end if
!            end do
!         end if

         if( &
              & (exp%pred%name .eq. 'wasCalled') .or. &
              & (exp%pred%name .eq. 'wasCalledOnce') .or. &
              & (exp%pred%name .eq. 'wasNotCalled') &
              & )then
            ok = .true.
            nCalls = 0
            do iReg=1,this%lastRegistration
               reg => this%callRegistry(iReg)
               if(exp%subj%name .eq. reg%subj%name) then
                  if(trim(reg%pred%name).eq.'wasCalled')then
                     nCalls=nCalls+1
                  end if
               end if
            end do
            if(exp%pred%name .eq. 'wasCalled')then
               ok = nCalls.ge.1
            else if(exp%pred%name .eq. 'wasCalledOnce')then
               ok = nCalls.eq.1
            else if(exp%pred%name .eq. 'wasNotCalled')then
               ok = nCalls.eq.0
            end if

         end if

         if(.not.ok)then
            call throw('             "'// &
                 & trim(exp%subj%name)//'" "'//trim(exp%pred%name)//'" does not hold.')
         end if
      end do

      ! call throw('exception%verify: Not implemented')
      
    end subroutine verify

! TODO: Is this the correct place to keep/use/verify expectations?  Or should we be more like
! exceptions?
!
   subroutine new_verify(this)
      use Exception_mod
      class (MockRepository), intent(inout), target :: this
      integer iExp, iReg
      class (Expectation), pointer :: exp, reg
      logical ok
      integer nCalls

      call throw('MockRepository::verify not implemented.')
      return
      
      do iExp=1,this%lastExpectation  ! 'with' syntax?
         exp => this%Expectations(iExp)
         ok = .false.
!         ok = exp%verify()
      end do

      call throw('EXCEPTION::MockRepository%new_verify: Not implemented')

    end subroutine new_verify

    integer function countTimesCalled(this,subjectName) result(nCalls)
      class (MockRepository), intent(inout), target :: this
      character(*), intent(in) :: subjectName
! multiple      integer                  :: nCalls

      class (Expectation), pointer :: reg

      integer :: iReg
      
      nCalls = 0

      ! iReg is the "found".
      do iReg=1,this%lastRegistration
         reg => this%callRegistry(iReg)
         if(subjectName .eq. reg%subj%name) then
            if(trim(reg%pred%name).eq.'wasCalled')then
               nCalls=nCalls+1
            end if
         end if
      end do

    end function countTimesCalled

   
end module MockRepository_mod
