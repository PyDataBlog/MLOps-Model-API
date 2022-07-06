! Read numbers from a file
program fileread

    implicit none

    double precision                :: a, b, c
    double precision, dimension(3)  :: M
    integer                         :: file_read_id=10, file_write_id=20

    ! Open and read the file
    open( unit=file_read_id, file='p05_data.txt' )
    read(file_read_id,*) a, b, c
    close( file_read_id )
    write(*,*) a, b, c

    ! We have reached EOF, reload and read into matrix
    ! Note: We get an error if the size of M is > # values in text file
    open( unit=file_read_id, file='p05_data.txt' )
    read(file_read_id,*) M
    close( file_read_id )
    write(*,*) M

    ! Write to file
    open( unit=file_write_id, file='p05_write.txt' )
    write( file_write_id,* ) a,b,c
    close( file_write_id )

end program fileread
