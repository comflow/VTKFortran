!< VTK file XMl writer, appended.
module vtk_fortran_vtk_file_xml_writer_appended
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file XMl writer, appended.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
use vtk_fortran_dataarray_encoder
use vtk_fortran_parameters
use vtk_fortran_vtk_file_xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: xml_writer_appended
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(xml_writer_abstract) :: xml_writer_appended
  !< VTK file XML writer, appended.
  type(string) :: encoding      !< Appended data encoding: "raw" or "base64".
  integer(I4P) :: scratch=0_I4P !< Scratch logical unit.
  contains
    ! deferred methods
    procedure, pass(self) :: initialize                 !< Initialize writer.
    procedure, pass(self) :: finalize                   !< Finalize writer.
    procedure, pass(self) :: write_dataarray1_rank1_up  !< Write dataarray 1, rank 1, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank2_up  !< Write dataarray 1, rank 2, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank3_up  !< Write dataarray 1, rank 3, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank4_up  !< Write dataarray 1, rank 4, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray_appended   !< Write appended.
    ! private methods
    procedure, pass(self), private :: ioffset_update     !< Update ioffset count.
    procedure, pass(self), private :: open_scratch_file  !< Open scratch file.
    procedure, pass(self), private :: close_scratch_file !< Close scratch file.
    generic, private :: write_on_scratch_dataarray =>          &
                        write_on_scratch_dataarray1_rank1,     &
                        write_on_scratch_dataarray1_rank2,     &
                        write_on_scratch_dataarray1_rank3,     &
                        write_on_scratch_dataarray1_rank4                   !< Write dataarray.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank1     !< Write dataarray, data 1 rank 1.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank2     !< Write dataarray, data 1 rank 2.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank3     !< Write dataarray, data 1 rank 3.
    procedure, pass(self), private :: write_on_scratch_dataarray1_rank4     !< Write dataarray, data 1 rank 4.
endtype xml_writer_appended

interface
   ! extern "C" int compress_memory(void *in_data, size_t in_data_size, void **out_data, size_t *out_data_size)
   integer(c_int) function compress_memory(in_data, in_data_size, out_data, out_data_size) bind(C,name="compress_memory")
   use iso_c_binding
   type(c_ptr),         intent(in), value :: in_data
   integer(c_size_t),   intent(in), value :: in_data_size
   type(c_ptr),         intent(in)        :: out_data
   integer(c_size_t),   intent(in)        :: out_data_size
   end function
end interface

!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, mesh_kind) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self          !< Writer.
  character(*),               intent(in)           :: format        !< File format: ASCII.
  character(*),               intent(in)           :: filename      !< File name.
  character(*),               intent(in)           :: mesh_topology !< Mesh topology.
  integer(I4P),               intent(in), optional :: nx1           !< Initial node of x axis.
  integer(I4P),               intent(in), optional :: nx2           !< Final node of x axis.
  integer(I4P),               intent(in), optional :: ny1           !< Initial node of y axis.
  integer(I4P),               intent(in), optional :: ny2           !< Final node of y axis.
  integer(I4P),               intent(in), optional :: nz1           !< Initial node of z axis.
  integer(I4P),               intent(in), optional :: nz2           !< Final node of z axis.
  character(*),               intent(in), optional :: mesh_kind     !< Kind of mesh data: Float64, Float32, ecc.
  integer(I4P)                                     :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = 'appended'
  self%encoding = format
  self%encoding = self%encoding%upper()
  select case(self%encoding%chars())
  case('RAW')
    self%encoding = 'raw'
  case('BINARY-APPENDED')
    self%encoding = 'base64'
  endselect
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
  self%ioffset = 0
  call self%open_scratch_file
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  function finalize(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  integer(I4P)                              :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(name=self%topology%chars())
  call self%write_dataarray_appended
  call self%write_end_tag(name='VTKFile')
  call self%close_xml_file
  call self%close_scratch_file
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  elemental subroutine ioffset_update(self, n_byte)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update ioffset count.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self  !< Writer.
  integer(I8P),               intent(in)    :: n_byte    !< Number of bytes saved.
  integer(I8P)                              :: increment
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  increment = 0_I8P
  if (self%encoding=='raw') then
    if (self%header_type==I4P) then
       increment = BYI4P + n_byte
  else
       increment = BYI8P + n_byte
  endif
  else
    if (self%header_type==I4P) then
       increment = ((n_byte + BYI4P + 2_I4P)/3_I4P)*4_I4P
    else
       increment = ((n_byte + BYI8P + 2_I4P)/3_I4P)*4_I4P
  endif
  endif
  self%ioffset = self%ioffset + increment
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine ioffset_update

  subroutine open_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Open scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(newunit=self%scratch, &
       form='UNFORMATTED',   &
       access='STREAM',      &
       action='READWRITE',   &
       status='SCRATCH',     &
       iostat=self%error)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine open_scratch_file

  subroutine close_scratch_file(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Close scratch file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  close(unit=self%scratch, iostat=self%error)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine close_scratch_file

  function write_dataarray1_rank1_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  class(*),                   intent(in)           :: x(1:)          !< Data variable.
  integer(I4P),               intent(in), optional :: n_components   !< Number of components.
  integer(I4P),               intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components_  !< Number of components.
  integer(I4P)                                     :: n_tuples_      !< Number of tuples.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = type2string(x(1))

  if (present(n_components)) then
    n_components_ = n_components
  else
    n_components_ = size(x, dim=1)
  endif

  if (present(n_tuples)) then
    n_tuples_ = n_tuples
  else
    n_tuples_ = size(x, dim=1)
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, n_components=n_components_, data_name=data_name, &
                                         n_tuples=n_tuples_)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_up

  function write_dataarray1_rank2_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  class(*),                   intent(in)           :: x(1:,1:)       !< Data variable.
  integer(I4P),               intent(in), optional :: n_components   !< Number of components.
  integer(I4P),               intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components_  !< Number of components.
  integer(I4P)                                     :: n_tuples_      !< Number of tuples.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = type2string(x(1,1))

  if (present(n_components)) then
    n_components_ = n_components
  else
    n_components_ = size(x, dim=1)
  endif

  if (present(n_tuples)) then
    n_tuples_ = n_tuples
  else
    n_tuples_ = size(x)/size(x,dim=1)
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, n_components=n_components_, data_name=data_name, &
                                         n_tuples=n_tuples_)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_up


  function write_dataarray1_rank3_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  class(*),                   intent(in)           :: x(1:,1:,1:)    !< Data variable.
  integer(I4P),               intent(in), optional :: n_components   !< Number of components.
  integer(I4P),               intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components_  !< Number of components.
  integer(I4P)                                     :: n_tuples_      !< Number of tuples.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = type2string(x(1,1,1))

  if (present(n_components)) then
    n_components_ = n_components
  else
    n_components_ = size(x, dim=1)
  endif

  if (present(n_tuples)) then
    n_tuples_ = n_tuples
  else
    n_tuples_ = size(x)/size(x,dim=1)
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, n_components=n_components_, data_name=data_name, &
                                         n_tuples=n_tuples_)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_up

  function write_dataarray1_rank4_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout)        :: self         !< Writer.
  character(*),               intent(in)           :: data_name    !< Data name.
  class(*),                   intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P),               intent(in), optional :: n_components   !< Number of components.
  integer(I4P),               intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                     :: error        !< Error status.
  character(len=:), allocatable                    :: data_type    !< Data type.
  integer(I4P)                                     :: n_components_  !< Number of components.
  integer(I4P)                                     :: n_tuples_      !< Number of tuples.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  data_type = type2string(x(1,1,1,1))

  if (present(n_components)) then
    n_components_ = n_components
  else
    n_components_ = size(x, dim=1)
  endif

  if (present(n_tuples)) then
    n_tuples_ = n_tuples
  else
    n_tuples_ = size(x)/size(x,dim=1)
  endif
  call self%write_dataarray_tag_appended(data_type=data_type, n_components=n_components_, data_name=data_name, &
                                         n_tuples=n_tuples_)
  call self%ioffset_update(n_byte=self%write_on_scratch_dataarray(x=x))
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_up

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Do nothing, ascii data cannot be appended.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self              !< Writer.
  type(string)                              :: tag_attributes    !< Tag attributes.
  integer(I8P)                              :: n_byte            !< Bytes count.
  character(len=2)                          :: dataarray_type    !< Dataarray type = R8,R4,I8,I4,I2,I1.
  integer(I8P)                              :: dataarray_dim     !< Dataarray dimension.
  real(R8P),    allocatable                 :: dataarray_R8P(:)  !< Dataarray buffer of R8P.
  real(R4P),    allocatable                 :: dataarray_R4P(:)  !< Dataarray buffer of R4P.
  integer(I8P), allocatable                 :: dataarray_I8P(:)  !< Dataarray buffer of I8P.
  integer(I4P), allocatable                 :: dataarray_I4P(:)  !< Dataarray buffer of I4P.
  integer(I2P), allocatable                 :: dataarray_I2P(:)  !< Dataarray buffer of I2P.
  integer(I1P), allocatable                 :: dataarray_I1P(:)  !< Dataarray buffer of I1P.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_start_tag(name='AppendedData', attributes='encoding="'//self%encoding%chars()//'"')
  write(unit=self%xml, iostat=self%error)'_'
  endfile(unit=self%scratch, iostat=self%error)
  rewind(unit=self%scratch, iostat=self%error)
  do
    call read_dataarray_from_scratch
    if (self%error==0) call write_dataarray_on_xml
    if (is_iostat_end(self%error)) exit
    if (self%error/=0) then
       print *, "ERROR while reading scratch file, no VTK file will be produced"
       exit
    endif
  enddo
  close(unit=self%scratch, iostat=self%error)
  write(unit=self%xml, iostat=self%error)end_rec
  call self%write_end_tag(name='AppendedData')
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine read_dataarray_from_scratch
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Read the current dataaray from scratch file.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    read(unit=self%scratch, iostat=self%error, end=10)n_byte, dataarray_type, dataarray_dim
    select case(dataarray_type)
    case('R8')
      if (allocated(dataarray_R8P)) deallocate(dataarray_R8P) ; allocate(dataarray_R8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R8P
    case('R4')
      if (allocated(dataarray_R4P)) deallocate(dataarray_R4P) ; allocate(dataarray_R4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_R4P
    case('I8')
      if (allocated(dataarray_I8P)) deallocate(dataarray_I8P) ; allocate(dataarray_I8P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I8P
    case('I4')
      if (allocated(dataarray_I4P)) deallocate(dataarray_I4P) ; allocate(dataarray_I4P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I4P
    case('I2')
      if (allocated(dataarray_I2P)) deallocate(dataarray_I2P) ; allocate(dataarray_I2P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I2P
    case('I1')
      if (allocated(dataarray_I1P)) deallocate(dataarray_I1P) ; allocate(dataarray_I1P(1:dataarray_dim))
      read(unit=self%scratch, iostat=self%error)dataarray_I1P
    case default
      self%error = 1
      write (stderr,'(A)')' error: bad dataarray_type = '//dataarray_type
      write (stderr,'(A)')' bytes = '//trim(str(n=n_byte))
      write (stderr,'(A)')' dataarray dimension = '//trim(str(n=dataarray_dim))
    endselect
    10 return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine read_dataarray_from_scratch

    subroutine write_dataarray_on_xml
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Write the current dataaray on xml file.
    !-------------------------------------------------------------------------------------------------------------------------------
    character(len=:), allocatable  :: code !< Dataarray encoded with Base64 codec.
    !-------------------------------------------------------------------------------------------------------------------------------
    if (self%encoding=='raw') then
      if (self%header_type==I4P) then
         write(unit=self%xml, iostat=self%error) int(n_byte,kind=I4P)
      else
         write(unit=self%xml, iostat=self%error) int(n_byte,kind=I8P)
      endif
      select case(dataarray_type)
      case('R8')
        write(unit=self%xml, iostat=self%error) dataarray_R8P
        deallocate(dataarray_R8P)
      case('R4')
        write(unit=self%xml, iostat=self%error) dataarray_R4P
        deallocate(dataarray_R4P)
      case('I8')
        write(unit=self%xml, iostat=self%error) dataarray_I8P
        deallocate(dataarray_I8P)
      case('I4')
        write(unit=self%xml, iostat=self%error) dataarray_I4P
        deallocate(dataarray_I4P)
      case('I2')
        write(unit=self%xml, iostat=self%error) dataarray_I2P
        deallocate(dataarray_I2P)
      case('I1')
        write(unit=self%xml, iostat=self%error) dataarray_I1P
        deallocate(dataarray_I1P)
      endselect
    else
      select case(dataarray_type)
      case('R8')
        code = encode_binary_dataarray(x=dataarray_R8P)
        write(unit=self%xml, iostat=self%error)code
      case('R4')
        code = encode_binary_dataarray(x=dataarray_R4P)
        write(unit=self%xml, iostat=self%error)code
      case('I8')
        code = encode_binary_dataarray(x=dataarray_I8P)
        write(unit=self%xml, iostat=self%error)code
      case('I4')
        code = encode_binary_dataarray(x=dataarray_I4P)
        write(unit=self%xml, iostat=self%error)code
      case('I2')
        code = encode_binary_dataarray(x=dataarray_I2P)
        write(unit=self%xml, iostat=self%error)code
      case('I1')
        code = encode_binary_dataarray(x=dataarray_I1P)
        write(unit=self%xml, iostat=self%error)code
      endselect
    endif
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine write_dataarray_on_xml
  endsubroutine write_dataarray_appended

  ! write_on_scratch_dataarray methods
  function write_on_scratch_dataarray1_rank1(self, x) result(n_byte)
  use iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_int, c_loc
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 1.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self   !< Writer.
  class(*),                   intent(in)    :: x(1:)  !< Data variable.
  integer(I8P)                              :: n_byte !< Number of bytes
  integer(I8P)                              :: nn     !< Number of elements.
  byte, pointer                             :: zlib_buffer(:)
  type(c_ptr)                               :: zlib_buffer_c
  integer(c_size_t)                         :: compressed_length
  integer(c_int)                            :: ierr
  character(len=2)                          :: code
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%compression==COMPRESSION_ZLIB) then
     n_byte = size(x, kind=I8P) * sizeof(x(1))
     print *, "is_contiguous: ", is_contiguous(x)
     ierr = compress_memory(in_data=c_loc(x), in_data_size=n_byte, &
                            out_data=zlib_buffer_c, out_data_size=compressed_length )
     call c_f_pointer(zlib_buffer_c, zlib_buffer, shape=[compressed_length])
     write(*,'("ZLIB relative size after compression of appended data: ",G0," % (",I0," -> ",I0,")")') &
        100. * real(compressed_length)/real(n_byte), n_byte, compressed_length
     write(unit=self%scratch, iostat=self%error) int(compressed_length,kind=I8P), 'I1', int(compressed_length,kind=I8P)
     write(unit=self%scratch, iostat=self%error) zlib_buffer
  else
     nn = size(x, kind=I8P)
     select type(x)
     type is(real(R8P))
       n_byte = nn*BYR8P
       code = 'R8'
     type is(real(R4P))
       n_byte = nn*BYR4P
       code = 'R4'
     type is(integer(I8P))
       n_byte = nn*BYI8P
       code = 'I8'
     type is(integer(I4P))
       n_byte = nn*BYI4P
       code = 'I4'
     type is(integer(I2P))
       n_byte = nn*BYI2P
       code = 'I2'
     type is(integer(I1P))
       n_byte = nn*BYI1P
       code = 'I1'
     class default
       return
     endselect
     write(unit=self%scratch, iostat=self%error) n_byte, code(1:2), nn
     write(unit=self%scratch, iostat=self%error) x
  endif

  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank1

  function write_on_scratch_dataarray1_rank2(self, x) result(n_byte)
  use iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_int, c_loc
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 2.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self     !< Writer.
  class(*),                   intent(in)    :: x(1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte   !< Number of bytes
  integer(I8P)                              :: nn       !< Number of elements.
  byte, pointer                             :: zlib_buffer(:)
  type(c_ptr)                               :: zlib_buffer_c
  integer(c_size_t)                         :: compressed_length
  integer(c_int)                            :: ierr
  character(len=2)                          :: code
  !------------------------------------------------------------------------------------------------------------------------------
  if (self%compression==COMPRESSION_ZLIB) then
     n_byte = size(x, kind=I8P) * sizeof(x(1,1))
     print *, "is_contiguous: ", is_contiguous(x)
     ierr = compress_memory(in_data=c_loc(x), in_data_size=n_byte, &
                            out_data=zlib_buffer_c, out_data_size=compressed_length )
     call c_f_pointer(zlib_buffer_c, zlib_buffer, shape=[compressed_length])
     write(*,'("ZLIB relative size after compression of appended data: ",G0," % (",I0," -> ",I0,")")') &
        100. * real(compressed_length)/real(n_byte), n_byte, compressed_length
     write(unit=self%scratch, iostat=self%error) int(compressed_length,kind=I8P), 'I1', int(compressed_length,kind=I8P)
     write(unit=self%scratch, iostat=self%error) zlib_buffer
  else
     nn = size(x, kind=I8P)
     select type(x)
     type is(real(R8P))
       n_byte = nn*BYR8P
       code = 'R8'
     type is(real(R4P))
       n_byte = nn*BYR4P
       code = 'R4'
     type is(integer(I8P))
       n_byte = nn*BYI8P
       code = 'I8'
     type is(integer(I4P))
       n_byte = nn*BYI4P
       code = 'I4'
     type is(integer(I2P))
       n_byte = nn*BYI2P
       code = 'I2'
     type is(integer(I1P))
       n_byte = nn*BYI1P
       code = 'I1'
     class default
       return
     endselect
     write(unit=self%scratch, iostat=self%error) n_byte, code(1:2), nn
     write(unit=self%scratch, iostat=self%error) x
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank2

  function write_on_scratch_dataarray1_rank3(self, x) result(n_byte)
  use iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_int, c_loc
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 3.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self        !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte      !< Number of bytes
  integer(I8P)                              :: nn          !< Number of elements.
  byte, pointer                             :: zlib_buffer(:)
  type(c_ptr)                               :: zlib_buffer_c
  integer(c_size_t)                         :: compressed_length
  integer(c_int)                            :: ierr
  character(len=2)                          :: code
  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%compression==COMPRESSION_ZLIB) then
     n_byte = size(x, kind=I8P) * sizeof(x(1,1,1))
     print *, "is_contiguous: ", is_contiguous(x)
     ierr = compress_memory(in_data=c_loc(x), in_data_size=n_byte, &
                            out_data=zlib_buffer_c, out_data_size=compressed_length )
     call c_f_pointer(zlib_buffer_c, zlib_buffer, shape=[compressed_length])
     write(*,'("ZLIB relative size after compression of appended data: ",G0," % (",I0," -> ",I0,")")') &
        100. * real(compressed_length)/real(n_byte), n_byte, compressed_length
     write(unit=self%scratch, iostat=self%error) int(compressed_length,kind=I8P), 'I1', int(compressed_length,kind=I8P)
     write(unit=self%scratch, iostat=self%error) zlib_buffer
  else
     nn = size(x, kind=I8P)
     select type(x)
     type is(real(R8P))
       n_byte = nn*BYR8P
       code = 'R8'
     type is(real(R4P))
       n_byte = nn*BYR4P
       code = 'R4'
     type is(integer(I8P))
       n_byte = nn*BYI8P
       code = 'I8'
     type is(integer(I4P))
       n_byte = nn*BYI4P
       code = 'I4'
     type is(integer(I2P))
       n_byte = nn*BYI2P
       code = 'I2'
     type is(integer(I1P))
       n_byte = nn*BYI1P
       code = 'I1'
     class default
       return
     endselect
     write(unit=self%scratch, iostat=self%error) n_byte, code(1:2), nn
     write(unit=self%scratch, iostat=self%error) x
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank3

  function write_on_scratch_dataarray1_rank4(self, x) result(n_byte)
  use iso_c_binding, only: c_ptr, c_size_t, c_f_pointer, c_int, c_loc
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write a dataarray with 1 components of rank 4.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_appended), intent(inout) :: self           !< Writer.
  class(*),                   intent(in)    :: x(1:,1:,1:,1:) !< Data variable.
  integer(I8P)                              :: n_byte         !< Number of bytes
  integer(I8P)                              :: nn             !< Number of elements.
  byte, pointer                             :: zlib_buffer(:)
  type(c_ptr)                               :: zlib_buffer_c
  integer(c_size_t)                         :: compressed_length
  integer(c_int)                            :: ierr
  character(len=2)                          :: code
  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%compression==COMPRESSION_ZLIB) then
     n_byte = size(x, kind=I8P) * sizeof(x(1,1,1,1))
     print *, "is_contiguous: ", is_contiguous(x)
     ierr = compress_memory(in_data=c_loc(x), in_data_size=n_byte, &
                            out_data=zlib_buffer_c, out_data_size=compressed_length )
     call c_f_pointer(zlib_buffer_c, zlib_buffer, shape=[compressed_length])
     write(*,'("ZLIB relative size after compression of appended data: ",G0," % (",I0," -> ",I0,")")') &
        100. * real(compressed_length)/real(n_byte), n_byte, compressed_length
     write(unit=self%scratch, iostat=self%error) int(compressed_length,kind=I8P), 'I1', int(compressed_length,kind=I8P)
     write(unit=self%scratch, iostat=self%error) zlib_buffer
  else
     nn = size(x, kind=I8P)
     select type(x)
     type is(real(R8P))
       n_byte = nn*BYR8P
       code = 'R8'
     type is(real(R4P))
       n_byte = nn*BYR4P
       code = 'R4'
     type is(integer(I8P))
       n_byte = nn*BYI8P
       code = 'I8'
     type is(integer(I4P))
       n_byte = nn*BYI4P
       code = 'I4'
     type is(integer(I2P))
       n_byte = nn*BYI2P
       code = 'I2'
     type is(integer(I1P))
       n_byte = nn*BYI1P
       code = 'I1'
     class default
       return
     endselect
     write(unit=self%scratch, iostat=self%error) n_byte, code(1:2), nn
     write(unit=self%scratch, iostat=self%error) x
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_on_scratch_dataarray1_rank4

endmodule vtk_fortran_vtk_file_xml_writer_appended
