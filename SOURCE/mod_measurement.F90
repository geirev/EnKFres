module mod_measurement
   type measurement
      integer*8 iw            ! Well number
      integer*8 i             ! Grid point i-index (poro/perm measurments)
      integer*8 j             ! Grid point j-index (poro/perm measurments)
      integer*8 k             ! Grid point k-index (poro/perm measurments)
      character(len=8) wgname ! Well character id
      character(len=8) keywrd ! Type of measurement
      real d                  ! Measurement value
      real var                ! Error variance of measurement
   end type measurement
end module mod_measurement

