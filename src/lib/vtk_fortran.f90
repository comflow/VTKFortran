!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
module vtk_fortran
!-----------------------------------------------------------------------------------------------------------------------------------
!< VTK_Fortran, pure Fortran (2003+) library to parse and emitt VTK files.
!-----------------------------------------------------------------------------------------------------------------------------------
use vtk_fortran_pvtk_file, only: pvtk_file
use vtk_fortran_vtk_file,  only: vtk_file
use vtk_fortran_vtm_file,  only: vtm_file
use vtk_fortran_parameters
!use vtk_fortran_utilities, only: xyz
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: pvtk_file
public :: vtk_file
public :: vtm_file
!public :: xyz
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule vtk_fortran
