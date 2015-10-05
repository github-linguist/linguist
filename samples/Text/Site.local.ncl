#define YmakeRoot $(DESTDIR)@prefix@
#define ManRoot $(DESTDIR)@mandir@
#define LibRoot $(DESTDIR)@libdir@/ncarg
#define SharePath $(DESTDIR)@datadir@

#define BuildWithF90 TRUE
#define IncSearch -I/usr/include/netcdf -I/usr/include/udunits2 -I/usr/include/freetype2 -I/usr/include/gdal
#define LibSearch -L@libdir@/hdf

#define BuildNetCDF4 1
#define NetCDF4lib -lnetcdf
#define BuildCAIRO 1
#define CAIROlib -lcairo -lfreetype
#define BuildGDAL 1
#define GDALlib -lgdal
#define BuildHDFEOS 0
#define BuildHDFEOS5 0
#define BuildTRIANGLE 0
#define HDFlib -lmfhdf -ldf -ljpeg -lz
#define HDF5lib -lhdf5_hl -lhdf5
#define BuildUdunits 1
#define UdUnitslib -ludunits2
