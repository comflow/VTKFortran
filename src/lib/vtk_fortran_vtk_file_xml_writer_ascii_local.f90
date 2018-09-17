!< VTK file XMl writer, ascii local.
module vtk_fortran_vtk_file_xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK file XMl writer, ascii local.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
use vtk_fortran_dataarray_encoder
use vtk_fortran_vtk_file_xml_writer_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(xml_writer_abstract) :: xml_writer_ascii_local
  !< VTK file XML writer, ascii local.
  contains
    ! deferred methods
    procedure, pass(self) :: initialize                 !< Initialize writer.
    procedure, pass(self) :: finalize                   !< Finalize writer.
    procedure, pass(self) :: write_dataarray1_rank1_up  !< Write dataarray 1, rank 1, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank2_up  !< Write dataarray 1, rank 2, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank3_up  !< Write dataarray 1, rank 3, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray1_rank4_up  !< Write dataarray 1, rank 4, unlimited polymorphic.
    procedure, pass(self) :: write_dataarray_appended   !< Write appended.
endtype xml_writer_ascii_local
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function initialize(self, format, filename, mesh_topology, nx1, nx2, ny1, ny2, nz1, nz2, mesh_kind) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: format        !< File format: ASCII.
  character(*),                  intent(in)           :: filename      !< File name.
  character(*),                  intent(in)           :: mesh_topology !< Mesh topology.
  integer(I4P),                  intent(in), optional :: nx1           !< Initial node of x axis.
  integer(I4P),                  intent(in), optional :: nx2           !< Final node of x axis.
  integer(I4P),                  intent(in), optional :: ny1           !< Initial node of y axis.
  integer(I4P),                  intent(in), optional :: ny2           !< Final node of y axis.
  integer(I4P),                  intent(in), optional :: nz1           !< Initial node of z axis.
  integer(I4P),                  intent(in), optional :: nz2           !< Final node of z axis.
  character(*),                  intent(in), optional :: mesh_kind     !< Kind of mesh data: Float64, Float32, ecc.
  integer(I4P)                                        :: error         !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%topology = trim(adjustl(mesh_topology))
  self%format_ch = format
  self%format_ch = self%format_ch%lower()
  call self%open_xml_file(filename=filename)
  call self%write_header_tag
  call self%write_topology_tag(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, mesh_kind=mesh_kind)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction initialize

  function finalize(self) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Finalize writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout) :: self  !< Writer.
  integer(I4P)                                 :: error !< Error status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%write_end_tag(name=self%topology%chars())
  call self%write_end_tag(name='VTKFile')
  call self%close_xml_file
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction finalize

  ! write_dataarray methods
  function write_dataarray1_rank1_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self         !< Writer.
  character(*),                  intent(in)           :: data_name    !< Data name.
  class(*),                      intent(in)           :: x(1:)          !< Data variable.
  integer(I4P),                  intent(in), optional :: n_components   !< Number of components.
  integer(I4P),                  intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                        :: error        !< Error status.
  character(len=:), allocatable                       :: data_type    !< Data type.
  integer(I4P)                                        :: n_components_  !< Number of components.
  integer(I4P)                                        :: n_tuples_      !< Number of tuples.
  character(len=:), allocatable                       :: code         !< Data variable encoded, ascii or Base64 codec.
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
  code = encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, n_components=n_components_, data_name=data_name, data_content=code, &
                                n_tuples=n_tuples_)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank1_up

  function write_dataarray1_rank2_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  class(*),                      intent(in)           :: x(1:,1:)       !< Data variable.
  integer(I4P),                  intent(in), optional :: n_components   !< Number of components.
  integer(I4P),                  intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components_  !< Number of components.
  integer(I4P)                                        :: n_tuples_      !< Number of tuples.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
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
    n_tuples_ = size(x, dim=1)
  endif
  code = encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, n_components=n_components_, data_name=data_name, data_content=code, &
                                n_tuples=n_tuples_)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank2_up

  function write_dataarray1_rank3_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  class(*),                      intent(in)           :: x(1:,1:,1:)    !< Data variable.
  integer(I4P),                  intent(in), optional :: n_components   !< Number of components.
  integer(I4P),                  intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components_  !< Number of components.
  integer(I4P)                                        :: n_tuples_      !< Number of tuples.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
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
    n_tuples_ = size(x, dim=1)
  endif
  code = encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, n_components=n_components_, data_name=data_name, data_content=code, &
                                n_tuples=n_tuples_)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank3_up


  function write_dataarray1_rank4_up(self, data_name, x, n_components, n_tuples) result(error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Write `<DataArray... NumberOfComponents="1"...>...</DataArray>` tag (unlimited polymorphic).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout)        :: self          !< Writer.
  character(*),                  intent(in)           :: data_name     !< Data name.
  class(*),                      intent(in)           :: x(1:,1:,1:,1:) !< Data variable.
  integer(I4P),                  intent(in), optional :: n_components   !< Number of components.
  integer(I4P),                  intent(in), optional :: n_tuples       !< Number of tuples.
  integer(I4P)                                        :: error         !< Error status.
  character(len=:), allocatable                       :: data_type     !< Data type.
  integer(I4P)                                        :: n_components_  !< Number of components.
  integer(I4P)                                        :: n_tuples_      !< Number of tuples.
  character(len=:), allocatable                       :: code          !< Data variable encoded, ascii or Base64 codec.
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
    n_tuples_ = size(x, dim=1)
  endif
  code = encode_ascii_dataarray(x=x)
  call self%write_dataarray_tag(data_type=data_type, n_components=n_components_, data_name=data_name, data_content=code, &
                                n_tuples=n_tuples_)
  error = self%error
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction write_dataarray1_rank4_up

  subroutine write_dataarray_appended(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Do nothing, ascii data cannot be appended.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(xml_writer_ascii_local), intent(inout) :: self !< Writer.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine write_dataarray_appended
endmodule vtk_fortran_vtk_file_xml_writer_ascii_local
