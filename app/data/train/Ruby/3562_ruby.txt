# == Schema Information
#
# Table name: historias_clinicas
#
#  id                                     :integer          not null, primary key
#  p_primer_nombre                        :string
#  p_segundo_nombre                       :string
#  p_primer_apellido                      :string
#  p_segundo_apellido                     :string
#  p_email                                :string
#  p_tipo_documento                       :string
#  p_documento                            :string
#  p_fecha_nacimiento                     :date
#  p_genero                               :string
#  p_profesion                            :string
#  p_tipo_sangre                          :string
#  p_nacionalidad                         :string
#  p_direccion                            :string
#  registro_medico                        :string
#  user_id                                :integer
#  p_ciudad                               :string
#  p_departamento                         :string
#  paciente_id                            :integer
#  a_auxiliar                             :string
#  aeropuerto_id                          :integer
#  cie10_id                               :integer
#  tipo_de_vuelo                          :string
#  a_cove                                 :string
#  a_movil_servicio                       :string
#  a_condicion                            :string
#  a_tipo_servicio                        :string
#  a_empresa                              :string
#  a_lugar_de_atencion                    :string
#  a_origen                               :string
#  a_destino                              :string
#  a_nombre_acompañante                   :string
#  a_parentesco                           :string
#  a_telefono                             :string
#  arl                                    :string
#  eps                                    :string
#  a_motivo_de_consulta                   :text
#  a_enfermedad_actual                    :text
#  b_neonatales                           :string
#  b_neonatales_descripcion               :text
#  b_patologicos                          :string
#  b_patologicos_descripcion              :text
#  b_quirurgicos                          :string
#  b_quirurgicos_descripcion              :text
#  b_farmacologicos                       :string
#  b_farmacologicos_descripcion           :text
#  b_alergicos                            :string
#  b_alergicos_descripcion                :text
#  b_toxicos                              :string
#  b_toxicos_descripcion                  :text
#  b_traumaticos                          :string
#  b_traumaticos_descripcion              :text
#  b_hospitalarios                        :string
#  b_hospitalarios_descripcion            :text
#  b_atep_laboral                         :string
#  b_atep_laboral_descripcion             :text
#  b_familiares                           :string
#  b_familiar_descripcion                 :text
#  b_inmunologicos                        :string
#  b_inmunologios_descripcion             :text
#  b_asociado_viajes_de_vuelo             :string
#  b_asociado_viajes_de_vuelo_descripcion :text
#  b_ciclo_menstrual                      :string
#  b_edad_gestacional                     :string
#  b_eco                                  :string
#  b_menarquia                            :string
#  b_fecha_ultima_citologia               :string
#  b_fecha_ultima_mamografia              :string
#  b_fum                                  :string
#  b_embarazo                             :string
#  b_g                                    :string
#  b_p                                    :string
#  b_c                                    :string
#  b_a                                    :string
#  b_m                                    :string
#  b_v                                    :string
#  b_e                                    :string
#  c_cardiovascular                       :string
#  c_cardiovascular_descripcion           :text
#  c_respiratorio                         :string
#  c_respiratorio_descripcion             :text
#  c_abdominal                            :string
#  c_abdominal_descripcion                :text
#  c_genito_urinario                      :string
#  c_genito_urinario_descripcion          :text
#  c_neurologico                          :string
#  c_neurologico_descripcion              :text
#  c_mental                               :string
#  c_mental_descripcion                   :text
#  c_organo_de_los_sentidos               :string
#  c_organo_de_los_sentidos_descripcion   :string
#  c_musculo_esqueletico                  :string
#  c_musculo_esqueletico_descripcion      :text
#  c_piel_y_faneras                       :string
#  c_piel_y_faneras_descripcion           :text
#  d_ta_mmgh_diastole                     :string
#  d_ta_mmgh_sistole                      :string
#  d_frecuencia_cardiaca                  :string
#  d_frecuencia_respiratoria              :string
#  d_sat_of_o_ambiente                    :string
#  d_sat_of_o_suplementario               :string
#  d_temperatura                          :string
#  d_peso_en_kilogramos                   :string
#  d_talla_en_centimetros                 :string
#  d_glucometria                          :string
#  d_cabeza                               :string
#  d_cabeza_descripcion                   :text
#  d_cuello                               :text
#  d_cuello_descripcion                   :text
#  d_caja_toracica                        :string
#  d_caja_toracica_descripcion            :text
#  d_corazon                              :string
#  d_corazon_descripcion                  :text
#  d_pulmones                             :string
#  d_pulmones_descripcion                 :text
#  d_abdomen                              :string
#  d_abdomen_descripcion                  :text
#  d_genito_urinario                      :string
#  d_genito_urinario_descripcion          :text
#  d_recto                                :string
#  d_recto_descripcion                    :text
#  d_extremidades                         :string
#  d_extremidades_descripcion             :text
#  d_piel_y_faneras                       :string
#  d_piel_y_faneras_descripcion           :text
#  d_musculo_esqueletico                  :string
#  d_musculo_esqueletico_descripcion      :text
#  d_mental                               :string
#  d_mental_descripcion                   :text
#  d_neurologico                          :string
#  d_neurologico_descripcion              :text
#  d_alteracion                           :string
#  d_apertura_ocular                      :integer
#  d_respuesta_verbal                     :integer
#  d_respuesta_motora                     :integer
#  d_otros_signos                         :string
#  d_escala_de_glasgow                    :integer
#  e_analisis                             :text
#  e_observaciones_recomendaciones        :text
#  e_evento_adverso                       :text
#  patologias                             :string
#  otras_patologias                       :string
#  accidentes_especiales                  :string
#  notificacion_obligatoria               :string
#  emergencias                            :string
#  e_estado_historia_clinica              :boolean
#  e_estado_paciente                      :string
#  e_glucometria1                         :string
#  e_hora1                                :string
#  e_glucometria2                         :string
#  e_hora2                                :string
#  e_electrocardiograma                   :text
#  created_at                             :datetime         not null
#  updated_at                             :datetime         not null
#  p_edad                                 :integer
#  p_tipo_edad                            :string
#  latitude                               :float
#  longitude                              :float
#  address                                :string
#

class HistoriaClinica < ActiveRecord::Base




  belongs_to :user
  belongs_to :paciente
  belongs_to :aeropuerto
  belongs_to :cie102
  belongs_to :cie10





  #belongs_to :origen_aeropuertos,  :primary_key => "id", :foreign_key => "a_orgien", :class_name  => "Aeropuerto"
  #belongs_to :destino_aeropuertos,  :primary_key => "id", :foreign_key => "a_destino", :class_name => "Aeropuerto"


  has_many :diagnosticos
  has_many :procedimientos
  has_many :notas_progreso


  accepts_nested_attributes_for :diagnosticos, reject_if: :all_blank, allow_destroy: true
  accepts_nested_attributes_for :procedimientos, reject_if: :all_blank, allow_destroy: true






  cattr_accessor :form_steps do
    %w(a_historia_clinica b_antecedentes c_revision_sistemas d_examen_fisico e_impresion_diagnostica)
  end

  attr_accessor :form_step





    reverse_geocoded_by :latitude, :longitude
    after_validation :reverse_geocode



  def required_for_step?(step)
    return true if form_step.nil?
    return true if self.form_steps.index(step.to_s) <= self.form_steps.index(form_step)
  end
   def self.search(search)
    where("p_primer_nombre like '%#{search}%'  or p_segundo_nombre like '%#{search}%'
    or p_primer_apellido like '%#{search}%'  or p_segundo_apellido like '%#{search}%'
    or p_email like '%#{search}%' or p_documento like '%#{search}%' or p_direccion like
    '%#{search}%'")
  end


   before_save do
      self.d_alteracion.gsub!(/[\[\]\"]/, "") if attribute_present?("d_alteracion")
      self.d_otros_signos.gsub!(/[\[\]\"]/, "") if attribute_present?("d_otros_signos")
   end


end
