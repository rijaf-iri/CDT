!!!!!!!!!!!!!!!!!  Cressman weighting method  !!!!!!!!!!!!!!!!!

subroutine cressman_interp(xyStn, valStn, nStn, xyGrd, nGrd, nmin, nmax,&
                           spheric, outM)
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: nStn, nGrd, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyStn(nStn, 2), valStn(nStn), xyGrd(nGrd, 2)
  real(rp), intent(out) :: outM(nGrd, 3)
  real(rp) :: out
  integer :: i

  outM(:, 1:2) = xyGrd
  do i = 1, nGrd
    call cressman_pixel(xyGrd(i, :), xyStn, valStn, nStn, nmin, nmax, spheric, out)
    outM(i, 3) = out
  end do
end subroutine cressman_interp

subroutine cressman_pixel(xyp, xym, values, n, nmin, nmax, spheric, out)
  implicit none
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: n, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyp(2), xym(n, 2), values(n)
  real(rp), intent(out) :: out

  real(rp) :: xdst(n), rmax
  integer :: order(n), npts
  real(rp) :: zs0(n)
  real(rp), dimension(:), allocatable :: zs, dst, Wk

  call distance_pixel(xyp, xym, n, nmin, nmax, spheric, xdst, rmax, npts, order)

  allocate(zs(npts), dst(npts))

  dst = xdst(1:npts)
  zs0 = values(order)
  zs = zs0(1:npts)

  if(all(dst < 1.e-10)) then
    out = sum(zs)/npts
    deallocate(zs, dst)
    return
  end if

  if(abs(maxval(zs) - minval(zs)) < 1.e-10) then
    out = zs(1)
    deallocate(zs, dst)
    return
  end if

  allocate(Wk(npts))

  Wk = (rmax**2 - dst**2)/(rmax**2 + dst**2)
  out = sum(Wk * zs) / sum(Wk)

  deallocate(zs, dst, Wk)
end subroutine cressman_pixel
