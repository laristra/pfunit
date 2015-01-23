!#include "reflection.h"
#define HERE print*,__LINE__,__FILE__

! Note: The SUT is not really the thing being mocked. Some
! reference or dependency in a SUT is what's being mocked.
! We'll likely change this at some point. 2014-0919-1541-45-UTC MLR
!
module SUT_InternalDependency_mod
  use AssertArraysSupport_mod, only: differenceReport, valuesReport
  use ThrowFundamentalTypes_mod, only: locationFormat
  use SourceLocation_mod
  
   implicit none
   private

   public :: SUT_InternalDependency

   type SUT_InternalDependency
      integer :: intComponent
   contains
      procedure :: method1
      procedure :: method2
      procedure :: method3
      procedure :: method4
   end type SUT_InternalDependency

contains

   subroutine method1(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method1

   subroutine method2(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method2

   subroutine method3(this)
     class (SUT_InternalDependency), intent(in) :: this
   end subroutine method3

   subroutine method4(this,i)
     class (SUT_InternalDependency), intent(in) :: this
     integer, intent(in) :: i
   end subroutine method4

end module SUT_InternalDependency_mod

module MockSUT_InternalDependency_mod
   use MockRepository_mod
   use SUT_InternalDependency_mod
   use Exception_mod
   implicit none
   private

   public :: MockSUT_InternalDependency
   public :: newMockSUT_InternalDependency

   type, extends(SUT_InternalDependency) :: MockSUT_InternalDependency
   contains
      procedure :: method1 ! mocked
      procedure :: method2 ! not mocked
      procedure :: method3 ! mocked
      procedure :: method4 ! mocked
   end type MockSUT_InternalDependency

contains

   function newMockSUT_InternalDependency() result(SUT_withMock)
     type (MockSUT_InternalDependency), allocatable :: SUT_withMock

     allocate(SUT_withMock)

   end function newMockSUT_InternalDependency

   subroutine method1(this)
      class (MockSUT_InternalDependency), intent(in) :: this
      call MockRepositoryPointer&
!           &%verifyArguments('MockSUT_InternalDependency%method1')
           &%registerMockCallBy('MockSUT_InternalDependency%method1')
! TODO: Do we want to register the call, and then search for an expectation on the arguments to verify?
!!!      call MockRepositoryPointer&
!!!           &%verifyArguments('MockSUT_InternalDependency%method1')

! TODO: fix

   end subroutine method1

   subroutine method2(this)
     class (MockSUT_InternalDependency), intent(in) :: this
     call this%SUT_InternalDependency%method2()  ! SUT_InternalDependency not abstract!
   end subroutine method2

   subroutine method3(this)
     class (MockSUT_InternalDependency), intent(in) :: this
      call MockRepositoryPointer&
           &%registerMockCallBy('MockSUT_InternalDependency%method3')
   end subroutine method3

! TODO: New verification scheme.
!
   subroutine method4(this,i)
     class (MockSUT_InternalDependency), intent(in) :: this
     integer, intent(in) :: i
      call MockRepositoryPointer&
           &%registerMockCallBy('MockSUT_InternalDependency%method4')
! TRY: Check the argument, register, and don't save the state.
!!!      call MockRepositoryPointer&
!!!           &%verifyArguments('MockSUT_InternalDependency%method4',i)
! TODO:  
!Q?      Throw an exception -- how to unwind?  Kick back?  
!Q?      The SUT won't know...  The mocks & the system will...  Wait for the SUT to return?
! The following might be the way to return from the SUT.
      if (anyExceptions()) return

! Call other stuff...
    end subroutine method4


end module MockSUT_InternalDependency_mod

module Test_MockRepository_mod
   use TestSuite_mod
   use MockRepository_mod
   use Assert_mod
   use SourceLocation_mod
   use Exception_mod, only : NULL_MESSAGE, catch, anyExceptions
   use Expectation_mod
   use Predicates_mod

!   use Assert_mod, only: assertEqual

   use SUT_InternalDependency_mod
   use MockSUT_InternalDependency_mod
   implicit none
   private

   public :: suite

   ! test that all registered objects are finalized (checked)

contains

!#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

   function suite()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('Test_MockRepository')

      call suite%addTest( &
           &   newTestMethod('testNoAction', &
           &                  testNoAction))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_NotCalled', &
           &                  testExpectMethod_NotCalled))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_IsCalled', &
           &                  testExpectMethod_IsCalled))
      call suite%addTest( &
           &   newTestMethod('testExpectMethod_CalledDifferentMethod', &
           &                  testExpectMethod_CalledDifferentMethod))

   end function suite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testNoAction()
      type (SUT_InternalDependency) :: object
      type (MockSUT_InternalDependency) :: SUT_withMockedMethod
      
      !!print *,1000
      SUT_withMockedMethod = newMockSUT_InternalDependency()
      call MockRepositoryPointer%verify()

   end subroutine testNoAction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testExpectMethod_NotCalled()

     !!print *,2000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   nWasCalled()))
     call internalProcedure() ! verification is when object is final-ized
     call MockRepositoryPointer%verify()

     call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')

   contains

      subroutine internalProcedure()
         type (MockSUT_InternalDependency) :: SUT_withMockedMethod
         
         !!print *,2100
         SUT_withMockedMethod = newMockSUT_InternalDependency()

      end subroutine internalProcedure

   end subroutine testExpectMethod_NotCalled

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

   subroutine testExpectMethod_IsCalled()

     use Exception_mod, only : anyExceptions

     !print *,3000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   nWasCalled()))
     call internalProcedure() ! verification is when object is final-ized

     call MockRepositoryPointer%verify()

   contains

      subroutine internalProcedure()
         type (MockSUT_InternalDependency) :: SUT_withMockedMethod

         !print *,3100
         SUT_withMockedMethod = newMockSUT_InternalDependency()
         call SUT_withMockedMethod%method1()

      end subroutine internalProcedure

   end subroutine testExpectMethod_IsCalled


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
   subroutine testExpectMethod_CalledDifferentMethod()

     !print *,4000
     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method1', &
          &   nWasCalled()))
      call internalProcedure() ! verification is when object is final-ized

      call MockRepositoryPointer%verify()

      call assertCatch('"MockSUT_InternalDependency%method1" "wasCalled" does not hold.')

   contains

      subroutine internalProcedure()
         type (MockSUT_InternalDependency) :: SUT_withMockedMethod

         !print *,4100
         SUT_withMockedMethod = newMockSUT_InternalDependency()
         call SUT_withMockedMethod%method3()

      end subroutine internalProcedure

   end subroutine testExpectMethod_CalledDifferentMethod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

! TODO:  Need a verify/catch to call between executions of mocks...
!        
!        call sut(mock1)
!        call catch & quit
!        call sut(mock2)
!        call catch & quit
!        call sut(mock3)
!        call catch & quit
!
! QUESTIONS:  What about calls like, call sut(mock1, mock2, mock3...)

   subroutine testArgumentConstraint1()

     type (MockSUT_InternalDependency) :: SUT_withMockedMethod

     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method4', &
          &   nWasCalled()))

     !!! TOD !!!
     !call MockRepositoryPointer%addExpectationThat( &
     !     & 'MockSUT_InternalDependency%method4', &
     !     & wasCalledWithOnly(999) )

     SUT_withMockedMethod = newMockSUT_InternalDependency()
     call SUT_withMockedMethod%method4(999)

     ! If an exception is caught, end NOW! Need to add some flow control here.
     call assertTrue(catch(NULL_MESSAGE)) ! For example...
     !??? call catchAndQuit ! Intermediate

     call SUT_withMockedMethod%method4(999)

     call MockRepositoryPointer%verify() ! Wrap up at end of tests...

     call assertCatch('"MockSUT_InternalDependency%method4" "wasCalled" does not hold.')

   end subroutine testArgumentConstraint1


   subroutine testArgumentConstraint2()

     type (MockSUT_InternalDependency) :: SUT_withMockedMethod

     call MockRepositoryPointer%add( &
          & ExpectationThat( &
          &   'MockSUT_InternalDependency%method4', &
          &   nWasCalled()))

     !!! TODO !!!
     !call MockRepositoryPointer%addExpectationThat( &
     !     & 'MockSUT_InternalDependency%method4', &
     !     & wasCalledWithOnly(-1) )

     SUT_withMockedMethod = newMockSUT_InternalDependency()

     call SUT_withMockedMethod%method4(999)

     ! If an exception is caught, end NOW! Need to add some flow control here.
     call assertTrue(catch(NULL_MESSAGE)) ! For example...
     ! call catchAndQuit ! Intermediate
     if (anyExceptions()) return

     call SUT_withMockedMethod%method4(-1)  ! Should this even be called?  If we're backing out?

     call MockRepositoryPointer%verify() ! Wrap up at end of tests...

     call assertCatch('"MockSUT_InternalDependency%method4" "wasCalled" does not hold.')

   end subroutine testArgumentConstraint2

!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!

  ! Check to see that the test result is as expected...
  subroutine assertCatch(string,location)
    use Params_mod
    use Exception_mod, only: getNumExceptions, Exception, catchNext
    use Assert_mod, only: assertEqual
    character(len=*), intent(in) :: string
    type (SourceLocation), optional, intent(in) :: location
    type (Exception) :: anException

    if (getNumExceptions() > 0) then
       anException = catchNext()

       !, 'exceptions do not match')
       call assertEqual(string,anException%getMessage()) ! ,message='Exception message test')
       if(present(location))then
          call assertEqual( &
               & location%lineNumber,anException%getLineNumber(), &
               & message='Source line number test')
          call assertEqual(location%fileName,anException%getFileName(), &
               & message='Source file name test')
       end if
    else
       !, 'missing exception')
       call assertEqual(string, ' ')
    end if
  end subroutine assertCatch


end module Test_MockRepository_mod
