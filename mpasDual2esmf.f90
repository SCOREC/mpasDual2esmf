! mpas2esmf.F90 - Create an ESMF and SCRIP file from an MPAS grid
!
!

module read_mesh

   contains
   
   subroutine read_mpas_mesh(filename, &
                             nCells, nVertices, maxEdges, &
                             latCell, lonCell, latVertex, lonVertex, nEdgesOnCell, cellsOnVertices, areaCell, &
                             sphere_radius, on_sphere)
   
      use netcdf
   
      implicit none
   
      character (len=*), intent(in) :: filename
      integer, intent(inout) :: nCells, nVertices, maxEdges
      double precision, dimension(:), pointer :: latCell, lonCell, latVertex, lonVertex, areaCell
      integer, dimension(:), pointer :: nEdgesOnCell
      integer, dimension(:,:), pointer :: cellsOnVertices
      double precision, intent(inout) :: sphere_radius
      integer, intent(inout) :: on_sphere

      integer :: on_sphere_str_len
      character (len = 80) :: on_sphere_str
      integer :: ncid, nCellsID, nVerticesID, maxEdgesID, latCellID, lonCellID, &
                 latVertexID, lonVertexID, nEdgesOnCellID, cellsOnVerticesID, areaCellID, status
   
      status = nf90_open(path=trim(filename), mode=nf90_nowrite, ncid=ncid)
      if (status /= nf90_noerr) then
          write(0,*) 'mpas2esmf: Error occured when opening MPAS grid: '//filename
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
   
      status = nf90_inq_dimid(ncid, 'nCells',    nCellsID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error when getting dimid of 'nCells'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_dimid(ncid, 'nVertices', nVerticesID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error when getting dimid of 'nVertices'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_dimid(ncid, 'maxEdges',  maxEdgesID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error when getting dimid of 'maxEdges'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
   
      status = nf90_inquire_dimension(ncid, nCellsID,    len=nCells)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire dimension of 'nCellsID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inquire_dimension(ncid, nVerticesID, len=nVertices)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire dimension of 'nVerticesID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inquire_dimension(ncid, maxEdgesID,  len=maxEdges)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire dimension of 'maxEdgesID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
   
      allocate(latCell(nCells))
      allocate(lonCell(nCells))
      allocate(latVertex(nVertices))
      allocate(lonVertex(nVertices))
      allocate(nEdgesOnCell(nCells))
      allocate(cellsOnVertices(3,nVertices))
      allocate(areaCell(nCells))
   
      status = nf90_inq_varid(ncid, 'latCell',   latCellID)
      status = nf90_inquire_dimension(ncid, maxEdgesID,  len=maxEdges)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'latCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'lonCell',   lonCellID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'lonCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'latVertex', latVertexID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'latVertex'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'lonVertex', lonVertexID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'lonVertex'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'nEdgesOnCell', nEdgesOnCellID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'nEdgesOnCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'cellsOnVertex', cellsOnVerticesID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'cellsOnVerticesID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inq_varid(ncid, 'areaCell', areaCellID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on inquire varid of 'areaCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
   
      status = nf90_get_var(ncid, latCellID, latCell)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'latCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, lonCellID, lonCell)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'lonCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, latVertexID, latVertex)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'latVertex'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, lonVertexID, lonVertex)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'lonVertex'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, nEdgesOnCellID, nEdgesOnCell)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'nEdgesOnCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, cellsOnVerticesID, cellsOnVertices)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'cellsOnVertices'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_var(ncid, areaCellID, areaCell)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get var of 'areaCell'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_get_att(ncid, NF90_GLOBAL, 'sphere_radius', sphere_radius)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get attribute of 'sphere_radius'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_inquire_attribute(ncid, NF90_GLOBAL, 'on_a_sphere', on_sphere_str_len)
      if(status /= 0 .or. len(on_sphere_str) < on_sphere_str_len) then
        print *, "Not enough space to put attribute values."
        stop
      end if
      status = nf90_get_att(ncid, NF90_GLOBAL, 'on_a_sphere', on_sphere_str)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error on get attribute of 'on_sphere'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      write(0,*) " on_sphere_str: ", on_sphere_str
      if (on_sphere_str .eq. "YES") then
        on_sphere = 1
      else if (on_sphere_str .eq. "NO") then
        on_sphere = 0
      else
        print *, "mpas2esmf: Error. Attribute on_a_sphere must be YES or NO, read", on_sphere_str
        stop
      end if
   
      status = nf90_close(ncid)
   
   end subroutine read_mpas_mesh
   
end module read_mesh


module write_desc

   contains

   subroutine write_esmf_mesh(filename, &
                              input_file, title, datestring, &
                              nCells, nVertices, maxEdges, &
                              nodeCoords, elementConn, nEdgesOnCell)
   
      use netcdf
   
      implicit none
   
      character (len=*), intent(in) :: filename
      character (len=*), intent(in) :: input_file
      character (len=*), intent(in) :: title
      character (len=*), intent(in) :: datestring
      integer, intent(inout) :: nCells, nVertices, maxEdges
      double precision, dimension(:,:), pointer :: nodeCoords
      integer, dimension(:,:), pointer :: elementConn
      integer, dimension(:), pointer :: nEdgesOnCell
   
   
      integer :: ncid, nVerticesID, nCellsID, maxNodePElementID, coordDimID, status
      integer :: nodeCoordsID, elementConnID, numElementConnID
      integer, dimension(1) :: id1
      integer, dimension(2) :: id2


      status = nf90_create(trim(filename), NF90_64BIT_OFFSET, ncid)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_create for esmf file"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_def_dim(ncid, 'nodeCount', nVertices, nVerticesID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_dim for 'nodeCount'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_def_dim(ncid, 'elementCount', nCells, nCellsID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_dim for 'elementCount'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_def_dim(ncid, 'maxNodePElement', maxEdges, maxNodePElementID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_dim for 'maxNodePElementID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_def_dim(ncid, 'coordDim', 2, coordDimID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_dim for 'coordDim'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      id2(1) = coordDimID
      id2(2) = nVerticesID
      status = nf90_def_var(ncid, 'nodeCoords', NF90_DOUBLE, id2, nodeCoordsID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_var for 'nodeCoords'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_put_att(ncid, nodeCoordsID, 'units', 'degrees')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for 'units' for 'nodeCoordsID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      id2(1) = maxNodePElementID
      id2(2) = nCellsID
      status = nf90_def_var(ncid, 'elementConn', NF90_INT, id2, elementConnID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_var for 'elementConn'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_put_att(ncid, elementConnID, 'long_name', &
                            'Node Indices that define the element connectivity')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for 'long_name' for 'elementConnID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, elementConnID, '_FillValue', -1)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for '_FillValue' for 'elementConnID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      id1(1) = nCellsID
      status = nf90_def_var(ncid, 'numElementConn', NF90_INT, id1, numElementConnID)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_def_var for 'numElementConn'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, numElementConnID, 'long_name', 'Number of nodes per element')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for 'long_name' for 'numElementConnID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      id2(1) = coordDimID
      id2(2) = nCellsID

      status = nf90_put_att(ncid, nodeCoordsID, 'units', 'degrees')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for 'units' for 'nodeCoordsID'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_put_att(ncid, NF90_GLOBAL, 'gridType', 'unstructured')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for global attribute 'gridType'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, NF90_GLOBAL, 'version', '0.9')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for global attribute 'version'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, NF90_GLOBAL, 'inputFile', trim(input_file))
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for global attribute 'inputFile'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, NF90_GLOBAL, 'timeGenerated', trim(datestring))
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for global attribute 'timeGenerated" 
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_att(ncid, NF90_GLOBAL, 'history', 'Created by the mpas2esmf utility')
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_att for global attribute 'history'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_enddef(ncid)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_enddef esmf file"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_put_var(ncid, nodeCoordsID, nodeCoords)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_var for 'nodeCoords'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_var(ncid, elementConnID, elementConn)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_var for 'elementConn'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
      status = nf90_put_var(ncid, numElementConnID, nEdgesOnCell)
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_put_var for 'numElementConn'"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if

      status = nf90_close(ncid) 
      if (status /= nf90_noerr) then
          write(0,*) "mpas2esmf: Error occured in nf90_close"
          write(0,*) trim(nf90_strerror(status))
          stop 
      end if
   
   end subroutine write_esmf_mesh
   
end module write_desc


program mpas2esmf

   use read_mesh
   use write_desc

   implicit none

   integer :: nCells, nVertices, maxEdges
   integer :: nDualCells, nDualVertices, maxDualEdges
   integer :: iCell, iVtx
   double precision, dimension(:), pointer :: latCell, lonCell, latVertex, lonVertex, grid_area
   integer, dimension(:), pointer :: nEdgesOnCell
   integer, dimension(:), pointer :: nEdgesOnDualCell
   integer, dimension(:,:), pointer :: cellsOnVertices, elementConn
   double precision, dimension(:,:), pointer :: nodeCoords

   double precision :: sphere_radius
   integer :: on_sphere
   integer, dimension(:), pointer :: grid_imask
   character (len=1024) :: input_file_name
   character (len=1024) :: title
   character (len=1024) :: datestring

   if (command_argument_count() /= 3) then
      write(0,*) 'Usage: mpas2esmf <mpas grid file> <title> <date>'
      stop
   end if

   call getarg(1,input_file_name)
   call getarg(2,title)
   call getarg(3,datestring)

   write(0,'(A)',advance='no') "mpas2esmf: Reading MPAS mesh ... "

   call read_mpas_mesh(trim(input_file_name), &
                       nCells, nVertices, maxEdges, &
                       latCell, lonCell, latVertex, lonVertex, nEdgesOnCell, cellsOnVertices, grid_area, &
                       sphere_radius, on_sphere)

   !in MPAS the primary mesh is polygons and the dual is triangles (alwasy?)
   maxDualEdges = 3 !hack... can this be read from source mpas mesh?
   nDualCells = nVertices
   nDualVertices = nCells
   allocate(nEdgesOnDualCell(nDualCells))
   nEdgesOnDualCell = 3
   write(0,*) "DONE!"
   write(0,*) "on_sphere", on_sphere
   write(0,*) "nDualVertices", nDualVertices
   write(0,*) "nDualCells", nDualCells

   !done, esmf, local var
   !y, nodeCount, nDualVertices
   !y, elementCount, nDualCells
   !y, maxNodePElement, maxDualEdges
   !y, coordDim, <hardcoded to 2 in writer>
   !y, numElementConn, nEdgesOnDualCell
   !n, nodeCoords, nodeCoords (sphere: [lat|lon]Cell)
   !n, elementConn(2D), 

   write(0,'(A)',advance='no') "mpas2esmf: Allocating and creating fields for ESMF file ... "

   allocate(nodeCoords(2,nDualVertices))
   nodeCoords(1,:) = lonCell(:)
   nodeCoords(2,:) = latCell(:)

   allocate(grid_imask(nCells))
   grid_imask(:) = 1

   allocate(elementConn(maxDualEdges,nDualCells))
   do iVtx=1,nVertices
      !mpas and esmf have opposite vertex ordering for triangles
      elementConn(1,iVtx) = cellsOnVertices(3,iVtx)
      elementConn(2,iVtx) = cellsOnVertices(2,iVtx)
      elementConn(3,iVtx) = cellsOnVertices(1,iVtx)
   end do

   write(0,*) "DONE!"
   write(0,'(A)',advance='no') "mpas2esmf: Writing ESMF files ... "

   call write_esmf_mesh('mpas_esmf.nc', &
                        input_file_name, title, datestring, &
                        nDualCells, nDualVertices, maxDualEdges, &
                        nodeCoords, elementConn, nEdgesOnDualCell)
                        

   write(0,*) "DONE!"

   deallocate(latCell)
   deallocate(lonCell)
   deallocate(latVertex) !not used
   deallocate(lonVertex) !not used
   deallocate(nEdgesOnCell) !not used
   deallocate(cellsOnVertices)

   deallocate(nodeCoords)
   deallocate(elementConn)
   deallocate(grid_imask)
   deallocate(grid_area)

   write(0,*) "mpas2emsf: FINISHED!"

end program mpas2esmf
