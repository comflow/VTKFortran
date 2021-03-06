!< Various utilities
module vtk_fortran_utilities
use penf,      only: I1P, I2P, I4P, I8P, R4P, R8P, R16P
implicit none
private
public :: type2string
public :: concat

interface type2string
  module procedure type2string_rank0
end interface

interface concat
  module procedure concat_rank1_I1P
  module procedure concat_rank1_I2P
  module procedure concat_rank1_I4P
  module procedure concat_rank1_I8P
  module procedure concat_rank1_R4P
  module procedure concat_rank1_R8P
  module procedure concat_rank2_I1P
  module procedure concat_rank2_I2P
  module procedure concat_rank2_I4P
  module procedure concat_rank2_I8P
  module procedure concat_rank2_R4P
  module procedure concat_rank2_R8P
  module procedure concat_rank3_I1P
  module procedure concat_rank3_I2P
  module procedure concat_rank3_I4P
  module procedure concat_rank3_I8P
  module procedure concat_rank3_R4P
  module procedure concat_rank3_R8P
end interface

contains

  subroutine concat_rank1_I1P( xyz, x, y, z )
  implicit none
  integer(I1P), intent(in)   :: x(:), y(:), z(:)
  integer(I1P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine
  subroutine concat_rank1_I2P( xyz, x, y, z )
  implicit none
  integer(I2P), intent(in)   :: x(:), y(:), z(:)
  integer(I2P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine
  subroutine concat_rank1_I4P( xyz, x, y, z )
  implicit none
  integer(I4P), intent(in)   :: x(:), y(:), z(:)
  integer(I4P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine
  subroutine concat_rank1_I8P( xyz, x, y, z )
  implicit none
  integer(I8P), intent(in)   :: x(:), y(:), z(:)
  integer(I8P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine
  subroutine concat_rank1_R4P( xyz, x, y, z )
  implicit none
  real(R4P), intent(in)   :: x(:), y(:), z(:)
  real(R4P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine
  subroutine concat_rank1_R8P( xyz, x, y, z )
  implicit none
  real(R8P), intent(in)   :: x(:), y(:), z(:)
  real(R8P), allocatable, intent(inout)  :: xyz(:,:)
  allocate( xyz( 3, size(x)) )
  xyz(1,:) = x
  xyz(2,:) = y
  xyz(3,:) = z
  end subroutine

  subroutine concat_rank2_I1P( xyz, x, y, z )
  implicit none
  integer(I1P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  integer(I1P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine
  subroutine concat_rank2_I2P( xyz, x, y, z )
  implicit none
  integer(I2P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  integer(I2P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine
  subroutine concat_rank2_I4P( xyz, x, y, z )
  implicit none
  integer(I4P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  integer(I4P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine
  subroutine concat_rank2_I8P( xyz, x, y, z )
  implicit none
  integer(I8P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  integer(I8P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine
  subroutine concat_rank2_R4P( xyz, x, y, z )
  implicit none
  real(R4P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  real(R4P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine
  subroutine concat_rank2_R8P( xyz, x, y, z )
  implicit none
  real(R8P), intent(in)   :: x(:,:), y(:,:), z(:,:)
  real(R8P), allocatable, intent(inout)  :: xyz(:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2)) )
  xyz(1,:,:) = x
  xyz(2,:,:) = y
  xyz(3,:,:) = z
  end subroutine

  subroutine concat_rank3_I1P( xyz, x, y, z )
  implicit none
  integer(I1P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  integer(I1P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine
  subroutine concat_rank3_I2P( xyz, x, y, z )
  implicit none
  integer(I2P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  integer(I2P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine
  subroutine concat_rank3_I4P( xyz, x, y, z )
  implicit none
  integer(I4P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  integer(I4P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine
  subroutine concat_rank3_I8P( xyz, x, y, z )
  implicit none
  integer(I8P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  integer(I8P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine
  subroutine concat_rank3_R4P( xyz, x, y, z )
  implicit none
  real(R4P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  real(R4P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine
  subroutine concat_rank3_R8P( xyz, x, y, z )
  implicit none
  real(R8P), intent(in)   :: x(:,:,:), y(:,:,:), z(:,:,:)
  real(R8P), allocatable, intent(inout)  :: xyz(:,:,:,:)
  allocate( xyz( 3, size(x,dim=1), size(x,dim=2), size(x,dim=3)) )
  xyz(1,:,:,:) = x
  xyz(2,:,:,:) = y
  xyz(3,:,:,:) = z
  end subroutine

  function type2string_rank0(x) result(str)
  implicit none
  class(*), intent(in)             :: x
  character(len=:), allocatable    :: str

  select type(x)
  type is(integer(I1P))
     str = 'Int8'
  type is(integer(I2P))
     str = 'Int16'
  type is(integer(I4P))
     str = 'Int32'
  type is(integer(I8P))
     str = 'Int64'
  type is(real(R4P))
     str = 'Float32'
  type is(real(R8P))
     str = 'Float64'
  !type is(real(R16P))
  !   str = 'Float128'
  class default
     str = '?'
  end select
  end function type2string_rank0

end module vtk_fortran_utilities