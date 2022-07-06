import { Component } from '@angular/core';
import { constObjConfig } from "../../shared/config";

import { SeoService } from "../../shared/seo.service";

@Component({
    selector: 'app-tmj-symptoms',
    template: `
        <section id="wrapper">
            <div id="maincontent" class="group">
                <div class="col g12">
                    <div class="conCon">
                        <div class="tmjMedia"><img src="{{ strAssetLocation }}/images/tmj-sublink-buttons.png" alt="tmj causes and symptoms" width="251" height="156" usemap="#Map4"/>
                            <map name="Map4">
                            <area shape="rect" coords="2,3,245,47" href="#causes">
                            <area shape="rect" coords="3,51,245,97" href="#symptoms">
                            <area shape="rect" coords="2,102,244,147" href="#treatment">
                            </map><br>
                            
                            <video class="tmjVideo" controls preload="auto">
                            <source src="{{ strAssetLocation }}/videos/tmj.mp4" type="video/mp4">
                            <source src="{{ strAssetLocation }}/videos/tmj.webm" type="video/webm">
                                <source src="{{ strAssetLocation }}/videos/tmj.ogv" type="video/ogg">
                                Your browser does not support the video tag.
                            </video>
                        </div><!-- /tmjMedia -->

                        <h1>TMJ / TMD</h1>

                        <p>The  Temporomandibular Joint is represented by the abbreviation TMJ and symbolizes  the first three anatomic areas of the head.</p>
                        
                        <p>
                            T = Temporal bone (skull)<br />
                            M = Mandible (lower jawbone)<br />
                            J = Joint (connects the two bones)
                        </p>

                        <p>
                            The  Temporomandibular Joint acts like a sliding hinge, connecting your jawbone to  your skull. This joint is unique due to its two joints; one on either side of  the head that connect to the lower mandible (jawbone). The TMJ joints work  together, whereas other joints work independently. When these joints break  down, it can be debilitating. The result of their breaking down is referred to  as TMD. TMJ disorders can cause pain in your jaw joint and in the muscles that  control jaw movement.
                        </p>
                        
                        <p>
                            The  exact cause of a person's TMJ disorder is often difficult to determine. Your  pain may be due to a combination of problems, such as arthritis or jaw injury.  Trauma induced TMJ disorders are found to be the statistic majority for  patients. As a response mechanism to pain of any kind, people frequently clench  or grind their teeth. This may be conscious or unconscious but it is not  normal. It should be evaluated to determine why it is happening before it  creates a micro trauma injury to the jaw joint. In most cases, the pain and  discomfort associated with TMJ disorders can be alleviated with proper  diagnosis and non-surgical treatments. Severe TMJ disorders may require  surgical repair.<br>
                        </p>

                        <p>Although  not as commonly used, disorders (improper function &amp; pain) of the  temporomandibular joint (TMD) may be used.</p>
                        
                        <p>
                            T = Temporal bone<br />
                            M = Mandible (lower jaw)<br />
                            D = Disorder/Dysfunction
                        </p>

                        <div id="causes" class="space-top-40">
                            <h1>TMJ/TMD CAUSES</h1>
                            <p>The cause of TMJ  / TMD is not clear, but dentists believe that symptoms arise from problems with  the muscles of the jaw or with the parts of the joint itself.</p>
                            <p>Injury to the  jaw, temporomandibular joint, or muscles of the head and neck &#8211; such as from a  heavy blow or whiplash &#8211; can cause TMD. Other possible causes include:</p>

                            <ul style="margin-top:0px; margin-left:30px;" class="space-bottom-20">
                                <li>Teeth grinding</li>
                                <li>Automobile Accident</li>
                                <li>Sports Injury</li>
                                <li>Stress</li>
                                <li>Systemic diseases</li>
                                <li>Neuromuscular imbalances</li>
                                <li>Inflammation of the joint</li>
                                <li>Arthritis  or other inflammatory musculoskeletal disorders</li>
                                <li>Osteoarthritis (degenerative)</li>
                            </ul>

                            <p>Another common  cause involves displacement or dislocation of the disk that is located between  the jawbone and the socket. The disk can also develop a hole or perforation,  which can produce a grating sound with joint movement. </p>
                            <p><strong>TMJ/TMD disorders can occur if:</strong></p>
                            <p>The disk erodes or moves out of its proper alignment.</p>
                            <p>The joint's cartilage is damaged by arthritis.</p>
                            <p>The joint is damaged by a blow or other impact.</p>
                            <p>The muscles that stabilize the joint become fatigued from overwork, which can happen if you habitually clench or grind your teeth.</p>
                        </div><!-- /#causes -->


                        <div id="symptoms" class="space-top-40">
                            <h1>TMJ/TMD SYMPTOMS</h1>
                            
                            <p>TMJ / TMD can cause  severe pain and discomfort in people and can last for a short time or last  years. This condition is more common in  women than men and the average age is between 20 and 40 years old.</p>
                            <p><strong>Common symptoms of TMJ / TMD include:</strong></p>
                            
                            <ul style="margin-top:0px; margin-left:30px;" class="space-bottom-20">
                                <li>Pain  or tenderness in the face, jaw joint area, neck and shoulders, and in or around  the ear when you chew, speak, or open your mouth wide</li>
                                <li>Limited ability to open the mouth very wide</li>
                                <li>Jaws that get &quot;stuck&quot; or &quot;lock&quot; in the open or closed mouth position</li>
                                <li>Clicking, popping, or grating sounds in the jaw joint when opening or closing the mouth (which may or may not be accompanied by pain) or chewing</li>
                                <li>A feeling of the face being tired or sore muscles</li>
                                <li>Difficulty chewing &#8211; as if the upper and lower teeth are not fitting together properly</li>
                                <li>Swelling on the side of the face</li>
                            </ul>

                            <p>Other common  symptoms of TMJ / TMD include toothaches, headaches, neck aches, dizziness, ear  aches, hearing problems, upper shoulder pain, and ringing in the ears.</p>
                            <p>The disk can also  develop a hole or perforation, which can produce a grating sound with joint  movement cause pain when opening and closing the mouth.</p>
                            <p>Jaw, face and/or neck pain are common indicators of acute or chronic inflammation of the TMJ. This can cause significant pain around the TM joint itself but the discomfort can also spread and surface in places outside the TMJ area.</p>
                        </div><!-- /#symptoms -->

                        <div id="treatment" class="space-top-40">
                            <h1>TMJ Treatment</h1>

                            <p>A full  comprehensive exam is necessary to understand what is causing the patients pain  and discomfort. This evaluation is  achieved by reviewing the patients' medical history as well as a detailed  clinical exam to include jaw movement, skeletal structure and posture, airway  and dental health, as well as cranial and neurological examination and muscle  palpations.</p>
                            <p>The two  temporomandibular joints can differ not only in size and shape, but also in  function. It is possible to have a problem in one joint with the symptoms  expressed in the other joint. Pain can migrate from one side to the other  because of the complexity of this joint.</p>
                            <p>Nerves, tendons,  ligaments and muscles are all interconnected with the TMJ. When any one part  malfunctions, this complex system can be compromised from working together in  harmony. Therefore, a careful and thorough diagnosis is needed to determine the  root cause.</p>
                            <p>Finding  the right practitioner to identify the root cause of the pain can be difficult  as the TMJ transcends the boundaries between several health-care disciplines  including: dentistry, neurology, pulmonology, otolaryngology or  gastroenterology, chiropractic or physical therapy and psychology. Therefore,  patients may find treatment approaches that vary from medication regimens for  pain management to molding a night-guard (which can actually exacerbate  inflammation).</p>
                            <p>Dr.  Alkhoury's state-of-the-art practice focuses on a comprehensive evaluation for  diagnosis and non-invasive treatment therapies. His office is equipped with the  latest technologies to further diagnose TMJ problems, direct non-surgical  treatment and monitor treatment progress for maximum results.</p>
                        </div><!-- #treatment -->
                    </div><!-- /conCon -->
                </div><!-- /col -->
            </div><!-- /#maincontent -->
        </section><!-- /wrapper -->
    `,
    styles: [``]
})
export class TmjSymptomsComponent {
    strAssetLocation: string;
    strImages: string;
    
    constructor(seoService: SeoService) {
        this.strImages = constObjConfig.assets + "/images";
        this.strAssetLocation = constObjConfig.assets;

        // SEO
        seoService.setTitle("TMJ Symptoms - TMJ & Sleep Therapy Centre of San Francisco");
        seoService.setMetaDescription("Looking for a san francisco TMJ specialist or a sleep doctor? Our center offers neck, jaw and facial pain treatment and therapies in San Francisco's Bay area.");
    } // constructor
} // AppComponent
