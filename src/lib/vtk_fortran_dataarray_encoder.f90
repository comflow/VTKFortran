!< DataArray encoder, codecs: "ascii", "base64".
module vtk_fortran_dataarray_encoder
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file XMl writer, ascii local.
!-----------------------------------------------------------------------------------------------------------------------------------
use befor64
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: encode_ascii_dataarray
public :: encode_binary_dataarray
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface encode_ascii_dataarray
  !< Ascii DataArray encoder.
  module procedure encode_ascii_dataarray1_rank1_up, &
                   encode_ascii_dataarray1_rank2_up, &
                   encode_ascii_dataarray1_rank3_up, &
                   encode_ascii_dataarray1_rank4_up
endinterface encode_ascii_dataarray
interface encode_binary_dataarray
  !< Binary (base64) DataArray encoder.
  module procedure encode_binary_dataarray1_rank1_up, &
                   encode_binary_dataarray1_rank2_up, &
                   encode_binary_dataarray1_rank3_up, &
                   encode_binary_dataarray1_rank4_up
endinterface encode_binary_dataarray
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !< ascii encoder
  function encode_ascii_dataarray1_rank1_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),        intent(in)   :: x(1:) !< Data variable.
  character(len=:), allocatable :: code  !< Encoded base64 dataarray.
  integer(I4P)                  :: n     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  !do n=1, size(x, dim=1)
  !  code = code//str(n=x(n))
  !enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank1_up

  function encode_ascii_dataarray1_rank2_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),        intent(in)   :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  !do n2=1, size(x, dim=2)
  !  do n1=1, size(x, dim=1)-1
  !    code = code//str(n=x(n1, n2))//' '
  !  enddo
  !  code = code//' '//str(n=x(size(x, dim=1), n2))
  !enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank2_up

  function encode_ascii_dataarray1_rank3_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),        intent(in)   :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1          !< Counter.
  integer(I4P)                  :: n2          !< Counter.
  integer(I4P)                  :: n3          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  !do n3=1, size(x, dim=3)
  !  do n2=1, size(x, dim=2)
  !    do n1=1, size(x, dim=1)-1
  !      code = code//str(n=x(n1, n2, n3))//' '
  !    enddo
  !    code = code//' '//str(n=x(size(x, dim=1), n2, n3))
  !  enddo
  !enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank3_up

  function encode_ascii_dataarray1_rank4_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*),        intent(in)   :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code     !< Encoded base64 dataarray.
  integer(I4P)                  :: n1             !< Counter.
  integer(I4P)                  :: n2             !< Counter.
  integer(I4P)                  :: n3             !< Counter.
  integer(I4P)                  :: n4             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  code = ''
  !do n4=1, size(x, dim=4)
  !  do n3=1, size(x, dim=3)
  !    do n2=1, size(x, dim=2)
  !      do n1=1, size(x, dim=1)
  !        code = code//str(n=x(n1,n2,n3,n4))//' '
  !      enddo
  !    enddo
  !  enddo
  !enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_ascii_dataarray1_rank4_up

  !< binary encoder
  function encode_binary_dataarray1_rank1_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 1 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in)          :: x(1:) !< Data variable.
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_binary_dataarray1_rank1_up

  function encode_binary_dataarray1_rank2_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 2 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in)          :: x(1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = sizeof(x(1,1))
  call pack_data(a1=[int(nn*size(x), I4P)], a2=reshape(x, [size(x)]), packed=xp)
  call b64_encode(n=xp, code=code)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_binary_dataarray1_rank2_up

  function encode_binary_dataarray1_rank3_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 3 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in)          :: x(1:,1:,1:) !< Data variable
  character(len=:), allocatable :: code        !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)       !< Packed data.
  integer(I4P)                  :: nn          !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_binary_dataarray1_rank3_up

  function encode_binary_dataarray1_rank4_up(x) result(code)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Encode (Base64) a dataarray with 1 components of rank 4 (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(*), intent(in)          :: x(1:,1:,1:,1:) !< Data variable.
  character(len=:), allocatable :: code           !< Encoded base64 dataarray.
  integer(I1P),     allocatable :: xp(:)          !< Packed data.
  integer(I4P)                  :: nn             !< Number of elements.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  nn = size(x, dim=1)*size(x, dim=2)*size(x, dim=3)*size(x, dim=4)
  call pack_data(a1=[int(nn*BYR8P, I4P)], a2=reshape(x, [nn]), packed=xp)
  call b64_encode(n=xp, code=code)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction encode_binary_dataarray1_rank4_up

endmodule vtk_fortran_dataarray_encoder
