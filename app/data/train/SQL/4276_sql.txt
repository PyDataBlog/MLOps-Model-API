SELECT 
  ctl_establecimiento.nombre Establecimiento, 
  mnt_expediente.id Expediente,
  mnt_paciente.primer_nombre NombrePaciente,
  mnt_empleado.id CodigoEnfermera,
  mnt_empleado.nombre NombreEnfermera,
  enf_anotacion.observacion_anot Anotaciones
  
FROM 
  public.mnt_empleado, 
  public.ctl_establecimiento, 
  public.enf_anotacion, 
  public.mnt_expediente, 
  public.mnt_paciente
WHERE 
  mnt_empleado.id = enf_anotacion.id_emp_corr AND
  ctl_establecimiento.id = mnt_empleado.id_establecimiento AND
  enf_anotacion.id_expediente = mnt_expediente.id AND
  mnt_expediente.id_paciente = mnt_paciente.id;
