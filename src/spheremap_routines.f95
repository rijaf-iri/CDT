  !!!!!!!!!!!!!!!!! Spheremap Interpolation !!!!!!!!!!!!!!!!!

  subroutine spheremap_interp(xyStn, valStn, nStn, xyGrd, nGrd, nmin, nmax,&
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
      call spheremap_pixel(xyGrd(i, :), xyStn, valStn, nStn, nmin, nmax, spheric, out)
      outM(i, 3) = out
    end do
  end subroutine spheremap_interp

  subroutine spheremap_pixel(xyp, xym, values, n, nmin, nmax, spheric, out)
    implicit none
    integer, parameter :: rp = selected_real_kind(15)
    integer, intent(in) :: n, nmin, nmax
    integer, intent(in) :: spheric
    real(rp), intent(in) :: xyp(2), xym(n, 2), values(n)
    real(rp), intent(out) :: out

    real(rp) :: xdst(n), rmax
    integer :: order(n), npts, i, k, ninf
    real(rp) :: zs0(n), xys0(n, 2), v, cosd_jk, sind_jk
    integer, allocatable :: Skinf(:), mk(:)
    real(rp), dimension(:), allocatable :: zs, dst, Sk, sinf, Wk, Tk, dlon_jk, dlat_jk
    real(rp), dimension(:), allocatable :: dz_lonk, dz_latk, dst_kl, dlon_lk, dlat_lk
    real(rp), dimension(:), allocatable :: cos_theta_kl, cosd_kl, cosd_jl, sind_jl
    real(rp), dimension(:), allocatable :: xx_kl, yy_kl, deltaZ_k
    real(rp), dimension(:, :), allocatable :: xys1
    real(rp), parameter :: infini = huge(0.0_rp)
    real(rp), parameter :: pi = 4.0_rp * atan(1.0_rp)

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

    allocate(xys1(npts, 2), Sk(npts), Skinf(npts))
    allocate(Wk(npts), Tk(npts), mk(npts - 1), cos_theta_kl(npts - 1))
    allocate(xx_kl(npts - 1), yy_kl(npts - 1), dlat_jk(npts), dlon_jk(npts), deltaZ_k(npts))
    allocate(dz_lonk(npts), dz_latk(npts), dst_kl(npts - 1), dlat_lk(npts - 1), dlon_lk(npts - 1))
    allocate(cosd_kl(npts - 1), cosd_jl(npts - 1), sind_jl(npts - 1))

    xys0 = xym(order, :)
    xys1 = xys0(1:npts, :)

    Sk(1:npts) = 0.0_rp
    where(dst <= rmax/3.0_rp) Sk = 1.0_rp/dst
    where((dst > rmax/3.0_rp) .and. (dst <= rmax))
      Sk = ((dst/rmax) - 1.0_rp)**2 * (27.0_rp/(4.0_rp * rmax))
    end where

    if(any(Sk > infini)) then
      Skinf(1:npts) = 0
      where(.not. Sk > infini) Skinf = 1
      ninf = sum(Skinf)
      allocate(sinf(ninf))
      sinf = pack(Sk, .not. Sk > infini)
      where(.not. Sk > infini)
        Sk = 0.1_rp * (sinf - minval(sinf))/(maxval(sinf) - minval(sinf))
      elsewhere
        Sk = 1.0_rp
      end where
      deallocate(sinf, Skinf)
    end if

    Tk(1:npts) = 0.0_rp
    Wk(1:npts) = 0.0_rp

    do k = 1, npts
      if(k == 1) then
        mk = (/ (i, i = 2, npts) /)
      else if(k == npts) then
        mk = (/ (i, i = 1, npts - 1) /)
      else
        mk = (/ (i, i = 1, k - 1), (i, i = k + 1, npts) /)
      end if

      if (spheric == 1) then
        call cos_spheric_distance(xys1(k, :), xys1(mk, :), npts - 1, cosd_kl)
        cosd_jk = cos(dst(k))
        cosd_jl = cos(dst(mk))
        sind_jk = sin(dst(k))
        sind_jl = sin(dst(mk))
        cos_theta_kl = (cosd_kl - cosd_jk * cosd_jl)/(sind_jk * sind_jl)
      else
        xx_kl = (xys1(k, 1) - xyp(1)) * (xys1(mk, 1) - xyp(1))
        yy_kl = (xys1(k, 2) - xyp(2)) * (xys1(mk, 2) - xyp(2))
        cos_theta_kl = (xx_kl + yy_kl)/(dst(k) * dst(mk))
      end if
      Tk(k) = sum(Sk(mk) * (1 - cos_theta_kl))
      Wk(k) = (Sk(k)**2) * (1 + Tk(k) /sum(Sk(mk)))
    end do

    if (spheric == 1) then
      dlon_jk = (xyp(1) - xys1(:, 1)) * cos(xyp(2) * pi/180.0_rp)
      dlat_jk = xyp(2) - xys1(:, 2)
    else
      dlon_jk = xyp(1) - xys1(:, 1)
      dlat_jk = xyp(2) - xys1(:, 2)
    end if

    dz_lonk(1:npts) = 0.0_rp
    dz_latk(1:npts) = 0.0_rp

    do k = 1, npts
      if(k == 1) then
        mk = (/ (i, i = 2, npts) /)
      else if(k == npts) then
        mk = (/ (i, i = 1, npts - 1) /)
      else
        mk = (/ (i, i = 1, k - 1), (i, i = k + 1, npts) /)
      end if

      call distance_vector(xys1(k, :), xys1(mk, :), npts - 1, spheric, dst_kl)
      if (spheric == 1) then
        dlon_lk = (xys1(mk, 1) - xys1(k, 1)) * cos(xys1(mk, 2) * pi/180.0_rp)
        dlat_lk = xys1(mk, 2) - xys1(k, 2)
        dst_kl = 6378.388_rp * dst_kl
      else
        dlon_lk = xys1(mk, 1) - xys1(k, 1)
        dlat_lk = xys1(mk, 2) - xys1(k, 2)
      end if

      dz_lonk(k) = sum(Wk(mk) * (zs(mk) - zs(k)) * dlon_lk * (1/dst_kl**2))/sum(Wk(mk))
      dz_latk(k) = sum(Wk(mk) * (zs(mk) - zs(k)) * dlat_lk * (1/dst_kl**2))/sum(Wk(mk))
    end do

    v = 0.1 * (maxval(zs0) - minval(zs0)) / maxval(sqrt(dz_lonk**2 + dz_latk**2))
    deltaZ_k = (dz_lonk * dlon_jk + dz_latk * dlat_jk) * (v / (v + dst))

    out = sum(Wk * (zs + deltaZ_k))/sum(Wk)

    deallocate(zs, dst, Sk, Wk, Tk, dlon_jk, dlat_jk, mk)
    deallocate(dz_lonk, dz_latk, dst_kl, dlon_lk, dlat_lk)
    deallocate(cos_theta_kl, cosd_kl, cosd_jl, sind_jl)
    deallocate(xx_kl, yy_kl, deltaZ_k, xys1)
  end subroutine spheremap_pixel
