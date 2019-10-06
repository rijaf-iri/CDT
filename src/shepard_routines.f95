!!!!!!!!!!!!!!!!!  Modified Shepard interpolation  !!!!!!!!!!!!!!!!!

subroutine shepard_interp(xyStn, valStn, nStn, xyGrd, nGrd, nmin, nmax,&
                          spheric, p, outM)
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: nStn, nGrd, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyStn(nStn, 2), valStn(nStn), xyGrd(nGrd, 2), p
  real(rp), intent(out) :: outM(nGrd, 3)
  real(rp) :: out
  integer :: i

  outM(:, 1:2) = xyGrd
  do i = 1, nGrd
    call shepard_pixel(xyGrd(i, :), xyStn, valStn, nStn, nmin, nmax, spheric, p, out)
    outM(i, 3) = out
  end do
end subroutine shepard_interp

subroutine shepard_pixel(xyp, xym, values, n, nmin, nmax, spheric, p, out)
  implicit none
  integer, parameter :: rp = selected_real_kind(15)
  integer, intent(in) :: n, nmin, nmax
  integer, intent(in) :: spheric
  real(rp), intent(in) :: xyp(2), xym(n, 2), values(n), p
  real(rp), intent(out) :: out

  real(rp) :: xdst(n), rmax
  integer :: order(n), npts, ninf
  real(rp) :: zs0(n)
  real(rp), dimension(:), allocatable :: zs, dst, Wk, wi
  integer, allocatable :: winf(:)
  real(rp), parameter :: infini = huge(0.0_rp)

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

  allocate(Wk(npts), winf(npts))

  Wk = ((rmax - dst) / (rmax * dst))**p

  if(any(Wk > infini)) then
    winf(1:npts) = 0
    where(.not. Wk > infini) winf = 1
    ninf = sum(winf)
    allocate(wi(ninf))
    wi = pack(Wk, .not. Wk > infini)
    where(.not. Wk > infini)
      Wk = (1.0_rp - ((maxval(wi) - wi)/(maxval(wi) - minval(wi))))/2.0_rp
    elsewhere
      Wk = 1.0_rp
    end where
    deallocate(wi, winf)
  end if

  out = sum(Wk * zs) / sum(Wk)

  deallocate(zs, dst, Wk)
end subroutine shepard_pixel
